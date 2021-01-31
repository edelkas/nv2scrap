# Modules
require 'net/http'
require 'timeout'
require 'zlib'
require 'base64'
# Gems
require 'nokogiri'
require 'active_record'

EPISODE_S = 1      # Starting point of the episode scrape
EPISODE_E = 60000  # Finishing point of the episode scrape
LEVEL_S   = 1      # Starting point of the level scrape
LEVEL_E   = 350000 # Finishing point of the level scrape

ATTEMPTS  = 10     # Retries until score is skipped
COMPRESS  = true   # Compress demos before storing in db
DEBUG     = false  # Will show exception messages
EMPTY_E   = 67     # Size of an empty episode score
EMPTY_L   = 46     # Size of an empty level score
EPISODES  = 100
EPSIZE    = 5
MAX       = 26000  # Maximum score, 650 seconds, bigger is ignored
REFRESH   = 100    # Refresh rate of the console, in hertz
SCRAPE_E  = true   # Scrape episode scores
SCRAPE_L  = true   # Scrape level scores
TIMEOUT   = 5      # Seconds until score is retried
THREADS   = 10     # Concurrency, set to 1 to disable

CONFIG    = {
  'adapter'   => 'mysql2',
  'database'  => 'n',
  'pool'      => 2 * THREADS,
  'host'      => 'localhost',
  'username'  => 'root',
  'password'  => 'root',
  'encoding'  => 'utf8mb4',
  'collation' => 'utf8mb4_unicode_ci'
}

# Hackers won't even be added to the database.
# Cheaters will, but will be ignored from all functions.
IGNORED_HACKERS = [
  'sry4trbleIMdone',
  'alllan',
  'Marcao',
  'JoaoGCNunes',
  'getpucht',
  'haxyoscoreboard',
  'haxYOscoreboard',
  'asda',
  'dnawrkshp',
  'Cuppy33',
  'Yvsk',
  'PolakJamByc',
  'KOKOZUDO',
  'igotbored:(',
  'JC239',
  'ninjump',
  'luanplayer',
  'banan049',
  'managans'
]

IGNORED_CHEATERS = [
  'kryX-orange',
  'ANGERFIST',
  'L3X',
  'Bonzai',
  'naem',
  'crappitrash',
  'Goo',
  'Sp33dY',
  'pokemaniac1342',
  'ACEOFSPADEWINS',
  'Vegeta',
  'BAS3',
  'cheese_god',
  'fuckingyourdad',
  'fuckingyourmom',
  'fuckingcrappitrash',
  'fuckingyourbrother',
  'fuckingyoursister',
  'schwah',
  'VotedStraw61372',
  'Donald_J_Trump'
]

# Global variables to control the concurrency of the program
$count   = 0     # How many scores have been scraped so far
$players = THREADS.times.map{ |t| nil }
$indices = THREADS.times.map{ |t| 0 } # ID being parsed by each thread
$time    = 0     # Time of execution
$is_lvl  = true  # Are we scraping levels or episodes.
$msg     = false # Controls whether the messaging thread should be messaging
$fixing  = false # Are we patching or not
$total   = 0     # How many scores do we have to scrape or patch

$count_mutex   = Mutex.new
$player_mutex  = Mutex.new
$indices_mutex = Mutex.new

# < -------------------------------------------------------------------------- >
# < ---                         DATABASE SETUP                             --- >
# < -------------------------------------------------------------------------- >

class Episode < ActiveRecord::Base
  has_many :levels
  has_many :scores, as: :highscoreable

  def format
    self.id.to_s.rjust(2, "0")
  end
end

class Level < ActiveRecord::Base
  belongs_to :episode
  has_many :scores, as: :highscoreable

  def self.find(ep, lvl)
    Level.where(id: EPSIZE * ep + lvl)[0]
  end

  def ep
    episode.id
  end

  def lvl
    id % 5
  end

  def format
    "#{"%02d" % ep}-#{lvl}"
  end
end

class Score < ActiveRecord::Base
  belongs_to :player
  belongs_to :highscoreable, polymorphic: true
  has_one :demo

  def demo
    decode(Demo.where(id: self.id).first.demo.to_s)
  end
end

class Player < ActiveRecord::Base
  has_many :scores
end

class Demo < ActiveRecord::Base
  belongs_to :score
end

class Error < ActiveRecord::Base
end

class Config < ActiveRecord::Base
end

def setup_db
  puts("Initializing database...")
  ActiveRecord::Base.establish_connection(CONFIG)
  ActiveRecord::Base.connection.create_table :episodes do |t|
  end
  ActiveRecord::Base.connection.create_table :levels do |t|
    t.references :episode, index: true
  end
  ActiveRecord::Base.connection.create_table :scores do |t|
    t.integer :score_id
    t.references :player, index: true
    t.references :highscoreable, polymorphic: true, index: true
    t.integer :rank, index: true
    t.integer :score
  end
  ActiveRecord::Base.connection.create_table :errors do |t|
    t.integer :score_id
    t.string :highscoreable_type
  end
  ActiveRecord::Base.connection.create_table :players do |t|
    t.string :name
  end
  ActiveRecord::Base.connection.create_table :demos do |t|
    #t.references :score
    t.text :demo
  end
  ActiveRecord::Base.connection.create_table :configs do |t|
    t.string :key
    t.string :value
  end
  Config.find_or_create_by(key: "level_start",   value: LEVEL_S)
  Config.find_or_create_by(key: "level_end",     value: LEVEL_E)
  Config.find_or_create_by(key: "episode_start", value: EPISODE_S)
  Config.find_or_create_by(key: "episode_end",   value: EPISODE_E)
  (0..EPISODES - 1).each{ |ep|
    e = Episode.find_or_create_by(id: ep)
    (0..EPSIZE - 1).each{ |lvl|
      Level.find_or_create_by(id: EPSIZE * ep + lvl).update(
        episode: e
      )
    }
  }
  Config.find_or_create_by(key: "initialized", value: 1)
end

# < -------------------------------------------------------------------------- >
# < ---                          SCRAPING CODE                             --- >
# < -------------------------------------------------------------------------- >

def _pack(n, size)
  n.to_s(16).rjust(2 * size, "0").scan(/../).map{ |b|
    [b].pack('H*')[0]
  }.join.force_encoding("ascii-8bit")
end

def _unpack(bytes)
  if bytes.is_a?(Array) then bytes = bytes.join end
  bytes.unpack('H*')[0].scan(/../).join.to_i(16)
end

# This code is used to encode and decode demos in a compressed manner.
# We can manage a tenfold compression!
def demo_encode(demo)
  framecount = demo.split(':')[0]
  bytes = demo.split(':')[1].split('|').map(&:to_i).map{ |frame|
    7.times.map{ |p|
      _pack(((frame % 16 ** (p + 1) - frame % 16 ** p).to_f / 16 ** p).round, 1)
    }
  }.flatten
  Base64.strict_encode64(Zlib::Deflate.deflate(framecount + ":" + bytes.join, 9))
end

def demo_decode(code)
  bytes = Zlib::Inflate.inflate(Base64.strict_decode64(code))
  framecount = bytes.split(':')[0]
  bytes = bytes.split(':')[1].scan(/./m)
  frames = bytes.each_slice(7).to_a.map{ |chunk|
    chunk.each_with_index.map{ |frame, i|
      _unpack(frame) * 16 ** i
    }.sum
  }.join('|')
  framecount + ':' + frames
end

def encode(demo)
  if demo.class == String
    COMPRESS ? demo_encode(demo) : demo
  elsif demo.class == Array
    COMPRESS ? demo.map{ |d| demo_encode(d) }.join('&') : demo.join('&')
  end
end

def decode(code)
  if code.index('&').nil?
    COMPRESS ? demo_decode(code) : demo
  else
    COMPRESS ? code.split('&').map{ |c| demo_decode(c) } : code.split('&')
  end
end

def download(id)
  attempts ||= 0
  Net::HTTP.post_form(
    URI.parse("http://www.harveycartel.org/metanet/n/data13/get_#{$is_lvl ? "lv" : "ep"}_demo.php"),
    pk: id
  ).body
rescue
  if (attempts += 1) < ATTEMPTS
    retry
  else
    nil
  end
end

def parse(i, id)
  attempts ||= 0
  ret = nil
  begin
    Timeout::timeout(TIMEOUT) do
      ret = download(id)
    end
  rescue Timeout::Error
    if (attempts += 1) < ATTEMPTS
      retry
    end
  end

  empty = $is_lvl ? EMPTY_L : EMPTY_E
  if ret.nil? || ret.size < empty then raise end
  if ret.size == empty then return 0 end
  score = ret[/&score=(\d+)/,1].to_i
  name = ret[/&name=(.*?)&demo/,1].to_s.each_byte.map{ |b| (b < 32 || b > 126) ? nil : b.chr }.compact.join.strip

  if score <= MAX && !IGNORED_HACKERS.include?(name) # Ignore hackers
    s = Score.find_or_create_by(score_id: id, highscoreable_type: $is_lvl ? Level : Episode)
    $player_mutex.synchronize do
      $players[i] = Player.find_or_create_by(name: name)
    end
    s.update(
      score: score,
      highscoreable_id: ($is_lvl ? EPSIZE : 1) * ret[/&epnum=(\d+)/,1].to_i + ret[/&levnum=(\d+)/,1].to_i,
      player: $players[i]
    )
    demo = $is_lvl ? ret[/&demo=(.*)&epnum=/,1].to_s : ret.scan(/demo\d+=(.*?)&/).map(&:first)
    valid = $is_lvl ? !demo.index(':').nil? : !demo.map{ |d| d.index(':').nil? }.any?
    if valid
      Demo.find_or_create_by(id: s.id).update(
        demo: encode(demo)
      )
    end
    $count_mutex.synchronize do
      $count += 1
    end
  else
    if $fixing
      $count_mutex.synchronize do
        $count += 1
      end
    end
  end
  return 0
rescue => e
  return 1
end

def msg
  index = 0
  while true
    if $msg
      if $fixing
        if $count != index
          index = $count
          print("Parsing score #{$count} / #{$total} (ID #{$indices.min})...".ljust(80, " ") + "\r")
        end
      else
        min = $indices.min
        if min != index
          index = min
          print("Parsing score with ID #{index} / #{$is_lvl ? LEVEL_E : EPISODE_E}...".ljust(80, " ") + "\r")
        end
      end
      sleep(1.0 / REFRESH)
    end
  end
end

def __scrap(type, ids)
    $msg = true
    Thread.new{ msg }
    threads = THREADS.times.map{ |i|
    Thread.new do
      ids.each_with_index{ |id, j|
        if j % THREADS == i
          ret = parse(i, id)
          $indices_mutex.synchronize do
            $indices[i] = id
          end
          Config.find_by(key: "#{type.downcase}_start").update(value: $indices.min + 1)
          if ret != 0
            Error.find_or_create_by(score_id: id, highscoreable_type: $is_lvl ? "Level" : "Episode")
            open('LOG', 'a') { |f|
              f.puts "[ERROR] [#{Time.now}] #{$is_lvl ? "Level" : "Episode"} score with ID #{id} failed to download."
            }
            puts("[ERROR] When parsing score with ID #{id} / #{$is_lvl ? LEVEL_E : EPISODE_E}...".ljust(80, " "))
          else
            if $fixing
              error = Error.find_by(score_id: id, highscoreable_type: $is_lvl ? "Level" : "Episode")
              error.destroy if !error.nil?
            end
          end
        end
      }
    end
  }
  threads.each(&:join)
  $msg = false
  return 0
rescue
  return 1
end

def _scrap(type)
  nstart = Config.find_by(key: "#{type.downcase}_start").value.to_i || ($is_lvl ? LEVEL_S : EPISODE_S)
  nend = Config.find_by(key: "#{type.downcase}_end").value.to_i || ($is_lvl ? LEVEL_E : EPISODE_E)
  if nstart == nend then return 0 end
  $total = nend - nstart
  $fixing = false
  $indices = THREADS.times.map{ |t| nstart }
  if __scrap(type, (nstart..nend).to_a) != 0 then return 1 end
  Config.find_by(key: "#{type.downcase}_start").update(value: nend)
  return 0
rescue
  return 1
end

def scrape
  $time = Time.now
  $count = 0
  $fixing = false
  error = false
  if SCRAPE_L
    puts("Scraping levels.".ljust(80, " "))
    $is_lvl = true
    ret = _scrap("level")
    if ret != 0 then error = true end
  end
  if SCRAPE_E
    puts("Scraping episodes.".ljust(80, " "))
    $is_lvl = false
    ret = _scrap("episode")
    if ret != 0 then error = true end
  end
  if error
    puts("\r[ERROR] Scraping failed at some point.".ljust(80, " "))
  else
    puts("\r[INFO] Scraped #{$count} scores successfully in #{(Time.now - $time).round(3)} seconds.".ljust(80, " "))
  end
rescue Interrupt
  puts("\r[INFO] Scraper interrupted. Scrapped #{$count} scores in #{(Time.now - $time).round(3)} seconds.".ljust(80, " "))
rescue Exception
end

def diagnose
  puts("Finding corrupt scores...")
  scores = Score.where(score: nil)
           .or(Score.where(player_id: nil))
           .or(Score.where(highscoreable_id: nil))
  scores_lvl = scores.where(highscoreable_type: "Level").size
  scores_ep  = scores.where(highscoreable_type: "Episode").size
  puts("Finding missing scores...")
  errors     = Error.all
  errors_lvl = errors.where(highscoreable_type: "Level").size
  errors_ep  = errors.where(highscoreable_type: "Episode").size
  puts("Finding hacked scores...")
  hacks      = Player.where(name: IGNORED_HACKERS).map(&:id)
  hacks_lvl  = Score.where(player_id: hacks, highscoreable_type: "Level").size
  hacks_ep   = Score.where(player_id: hacks, highscoreable_type: "Episode").size
  puts("Finding duplicate scores...")
  duplis     = Score.select(
                            'player_id',
                            'count(player_id)',
                            'highscoreable_type',
                            'count(highscoreable_type)',
                            'highscoreable_id',
                            'count(highscoreable_id)'
                           )
                    .group(
                            'player_id',
                            'highscoreable_type',
                            'highscoreable_id'
                          )
                    .having(
                            'count(player_id) > 1',
                            'count(highscoreable_type) > 1',
                            'count(highscoreable_id) > 1'
                           )
  
  duplis_lvl = duplis.where(highscoreable_type: "Level").to_a.size
  duplis_ep  = duplis.where(highscoreable_type: "Episode").to_a.size
  score_ids = scores.map(&:id)  
  table = [
    [
      "ERRORS",
      "Levels",
      "Episodes",
      "Total"
    ],
    :sep,
    [
      "Corrupt scores",
      scores_lvl,
      scores_ep,
      scores_lvl + scores_ep
    ],
    [
      "Missing scores",
      errors_lvl,
      errors_ep,
      errors_lvl + errors_ep
    ],
    [
      "Hacked scores",
      hacks_lvl,
      hacks_ep,
      hacks_lvl + hacks_ep
    ],
    [
      "Duplicate scores",
      duplis_lvl,
      duplis_ep,
      duplis_lvl + duplis_ep
    ],
    :sep,
    [
      "Total",
      scores_lvl + errors_lvl + hacks_lvl + duplis_lvl,
      scores_ep + errors_ep + hacks_ep + duplis_ep,
      scores_lvl + scores_ep + errors_lvl + errors_ep + hacks_lvl + hacks_ep + duplis_lvl + duplis_ep
    ]
  ]
  puts make_table(table)
  puts("To fix the corrupt and missing scores run the 'patch' command.")
  puts("To remove the hacked scores run the 'sanitize' command.")
  puts("To remove the duplicate scores run the 'uniq' command.")
  puts("Note: If corrupt scores persist, they might just be corrupt in the server itself.")
rescue => e
  puts "An error occurred while diagnosing."
  puts e if DEBUG
end

def patch
  scores = Score.where(score: nil)
           .or(Score.where(player_id: nil))
           .or(Score.where(highscoreable_id: nil))
  errors = Error.all
  lvls = scores.where(highscoreable_type: "Level").map(&:score_id).map(&:to_i) +
         errors.where(highscoreable_type: "Level").map(&:score_id).map(&:to_i)
  eps =  scores.where(highscoreable_type: "Episode").map(&:score_id).map(&:to_i) +
         errors.where(highscoreable_type: "Episode").map(&:score_id).map(&:to_i)
  $fixing = true
  $count = 0
  $time = Time.now
  if SCRAPE_L
    puts("Patching levels.".ljust(80, " "))
    $is_lvl = true
    $total = lvls.size
    ret = __scrap("level", lvls)
  end
  if SCRAPE_E
    puts("Patching episodes.".ljust(80, " "))
    $is_lvl = false
    $total = eps.size
    ret = __scrap("episode", eps)
  end 
  $fixing = false
  puts("\r[INFO] Patched #{$count} scores in #{(Time.now - $time).round(3)} seconds.")
  puts("Please 'diagnose' again to verify the scores have been patched.")
  puts("If they persist, they might just be corrupt in the server itself.")
rescue => e
  puts "An error occurred while patching the scores."
  puts e if DEBUG
end

def sanitize
  players = Player.where(name: IGNORED_HACKERS)
  player_ids = players.map(&:id)
  puts("Found #{player_ids.size} hackers.")
  scores = Score.where(player_id: player_ids)
  score_ids = scores.map(&:id)
  puts("Found #{score_ids} hacked scores.")
  demos = Demo.where(id: score_ids)
  puts("Removing...")
  players.each{ |p| p.destroy }
  scores.each{ |s| s.destroy }
  demos.each{ |d| d.destroy }
  puts("Destroyed hacked scores.")
end

def uniq
  puts("Finding duplicate scores...")
  duplis = Score.select(
                        'player_id',
                        'count(player_id)',
                        'highscoreable_type',
                        'count(highscoreable_type)',
                        'highscoreable_id',
                        'count(highscoreable_id)'
                       )
                .group(
                       'player_id',
                       'highscoreable_type',
                       'highscoreable_id'
                      )
               .having(
                       'count(player_id) > 1',
                       'count(highscoreable_type) > 1',
                       'count(highscoreable_id) > 1'
                       )
  puts("Removing duplicate scores...")
  duplis.each{ |d|
    Score.where(
                player_id: d.player_id,
                highscoreable_type: d.highscoreable_type,
                highscoreable_id: d.highscoreable_id
               )
         .sort_by{ |s|
                   [-s.score, -s.score_id]
                 }[1..-1]
         .each{ |s|
                s.destroy
                Demo.find(s.id).destroy if !Demo.find(s.id).nil?
              }
  }
  puts("Done.")
end

def seed
  ActiveRecord::Base.transaction do
    [Level, Episode].each{ |type|
      type.all.each{ |l|
        print("Seeding #{type.to_s.downcase} #{l.format}...".ljust(80, " ") + "\r")
        l.scores.sort_by{ |s| [-s.score, s.score_id] }.each_with_index{ |s, r|
          s.update(rank: r)
        }
      }
    }
  end
  puts "Done"
end 

# < -------------------------------------------------------------------------- >
# < ---                             ANALYSIS                               --- >
# < -------------------------------------------------------------------------- >

def export(filename, content)
  File.write(filename, content)
  puts "Exported to '#{filename}' (#{content.length} bytes)."
end

def scores
  ep  = nil
  lvl = nil

  while ep.nil?  
    print("Episode > ")
    ep = STDIN.gets.chomp.to_i
    if ep < 0 || ep > 99
      puts "Episode should be between 0 and 99."
      ep = nil
    end
  end

  while lvl.nil?
    print("Level > ")
    lvl = STDIN.gets.chomp.to_i
    if lvl < 0 || lvl > 4
      puts "Level should be between 0 and 4."
      lvl = nil
    end
  end

  scores = Level.find(ep, lvl).scores.sort_by{ |s| -s.score }
  pad_rank  = scores.size.to_s.length
  pad_score = (scores[0].score.to_f / 40).to_i.to_s.length + 4

  File.write(
    "#{"%02d" % ep}-#{lvl}.txt",
    scores.each_with_index.map{ |s, i|
      "#{"%0#{pad_rank}d" % i} #{"%#{pad_score}.3f" % (s.score.to_f / 40)} #{s.player.name}"
    }.join("\n")
  )
  puts "Exported to \"#{"%02d" % ep}-#{lvl}.txt\""
end

def count
  [Level, Episode].each{ |type|
    table = type.all.map{ |l|
      print("Parsing #{type.to_s.downcase} #{l.format}...".ljust(80, " ") + "\r")
      [l.format, l.scores.size]
    }.sort_by{ |l, count| -count }
    table.prepend(:sep).prepend([type.to_s, "Completions"])
    export("count_#{type.to_s.downcase}.txt", make_table(table))
  }
end

def stats
  lvl_scores = Score.where(highscoreable_type: "Level")
  ep_scores  = Score.where(highscoreable_type: "Episode")
  tls = lvl_scores.where(rank: 0).pluck(:score).sum
  tes = ep_scores.where(rank: 0).pluck(:score).sum
  table = [
    [
      "",
      "Levels",
      "Episodes",
      "Total"
    ],
    :sep,
    [
      "Scores",
      lvl_scores.size,
      ep_scores.size,
      lvl_scores.size + ep_scores.size
    ],
    [
      "Players",
      "",
      "",
      Player.all.size
    ],
    [
      "Total Score",
      "%.3f" % (tls.to_f / 40),
      "%.3f" % (tes.to_f / 40),
      "%.3f" % ((tls - tes - 40 * 4 * 90 * 100).to_f / 40)
    ]
  ]
  puts make_table(table)
end

def total
  ["Level", "Episode"].each{ |type|
    puts("Computing Total #{type} Score boards...")
    range = type == "Level" ? (0..EPSIZE * EPISODES - 1) : (0..EPISODES - 1)
    table = Score.where(
              highscoreable_type: type,
              highscoreable_id: range
            )
         .group(:player_id)
         .sum(:score)
         .sort_by{ |id, t| -t }
         .each_with_index
         .map{ |p, r|
               player = Player.find(p[0])
               [
                 r,
                 player.name,
                 "%.3f" % (p[1].to_f / 40),
                 Score.where(
                   player_id: player.id,
                   highscoreable_type: type,
                   highscoreable_id: range
                 ).size
               ]
             }
    table.prepend(:sep)
    table.prepend(["Rank", "Player", type == "Level" ? "TLS" : "TES", "Scores"])
    export("#{type == "Level" ? "tls" : "tes"}.txt", make_table(table))
  }
rescue => e
  puts "An error occurred."
  puts e
end

# < -------------------------------------------------------------------------- >
# < ---                             STARTUP                                --- >
# < -------------------------------------------------------------------------- >

def setup
  ActiveRecord::Base.establish_connection(CONFIG)
  if Config.find_by(key: "initialized").value.to_i != 1 then setup_db end
  Config.find_or_create_by(key: "level_end").update(value: LEVEL_E)
  Config.find_or_create_by(key: "episode_end").update(value: EPISODE_E)
rescue ActiveRecord::ActiveRecordError
  setup_db
rescue
  return 1
end

# To use this you need to send either a matrix or :sep
def make_table(rows, sep_x = "=", sep_y = "|", sep_i = "x")
  text_rows = rows.select{ |r| r.is_a?(Array) }
  count = text_rows.map(&:size).max
  rows.each{ |r| if r.is_a?(Array) then r << "" while r.size < count end }
  widths = (0..count - 1).map{ |c| text_rows.map{ |r| r[c].to_s.length }.max }
  sep = widths.map{ |w| sep_i + sep_x * (w + 2) }.join + sep_i + "\n"
  table = sep.dup
  rows.each{ |r|
    if r == :sep
      table << sep
    else
      r.each_with_index{ |s, i|
        table << sep_y + " " + (s.is_a?(Numeric) ? s.to_s.rjust(widths[i], " ") : s.to_s.ljust(widths[i], " ")) + " "
      }
      table << sep_y + "\n"
    end
  }
  table << sep
  return table
end

def reset
  Config.find_or_create_by(key: "level_start").update(value: LEVEL_S)
  Config.find_or_create_by(key: "level_end").update(value: LEVEL_E)
  Config.find_or_create_by(key: "episode_start").update(value: EPISODE_S)
  Config.find_or_create_by(key: "episode_end").update(value: EPISODE_E)
end

def cls
  print("".ljust(80, " ") + "\r")
end

def commands
  {
    "scrape"   => "Scrapes the server and seeds the database.",
    "diagnose" => "Find erroneous scores.",
    "patch"    => "Fix erronous scores.",
    "sanitize" => "Remove hackers from database.",
    "uniq"     => "Remove duplicate scores.",
    "seed"     => "Calculates and fills 'rank' field of database.",
    "scores"   => "Show score leaderboard for a specific episode or level.",
    "count"    => "Sorts levels by number of completions.",
    "total"    => "Show total score leaderboards",
    "stats"    => "Shows statistics.",
    "reset"    => "Resets database config values (e.g. to scrape a-fresh).",
    "exit"     => "Exit the program."
  }
end

def help
  puts "DESCRIPTION: A tool to scrape N v1.4 scores and analyze them."
  puts "USAGE: ruby nscrap.rb [ARGUMENT]"
  puts "ARGUMENTS:"
  commands.sort_by{ |c| c[0] }.each{ |c|
    puts "  " + c[0].rjust(8, " ") + " - " + c[1]
  }
  puts "NOTES:"
  puts "  * MySQL with a database named '#{CONFIG['database']}' is needed."
end

def main
  puts("[INFO] N Scraper initialized (Using #{THREADS} threads).")
  setup
  puts("[INFO] Connection to database established.")
  command = (ARGV.size == 0 ? nil : ARGV[0])

  loop do
    if command.nil?
      cls
      print("Command > ")
      command = STDIN.gets.chomp
    end
    if !commands.keys.include?(command)
      help
      command = nil
      next
    end
    send(command)
    command = nil
  end
rescue Interrupt
rescue
  puts "An error occurred."
end

main
