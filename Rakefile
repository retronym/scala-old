# Ruby Rake file to allow automatic build testing on RunCodeRun.com

task :default => [:debug_java]

task :test do
  system("ant") || abort
end
