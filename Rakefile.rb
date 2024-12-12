desc "Build the application"
task :build do
    sh "cabal build"
end

task :default => [:build]

desc "Run the application"
task :run do
    sh "cabal exec aoc2024-exe"
end

desc "Run the unit tests"
task :test do
    sh "cabal test"
end