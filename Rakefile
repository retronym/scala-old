# Ruby Rake file to allow automatic build testing on RunCodeRun.com

task :default => [:debug_java]

task :debug_java do
  puts "JAVA_HOME => #{ENV['JAVA_HOME']}"
  puts ENV
  system! "java --version"
  system! "javac --version"
end

task :test do
  classpath = [
    File.join(".", "runcoderun", "ant.jar"),
    File.join(".", "runcoderun", "ant-launcher.jar"),
    File.join(".", "runcoderun", "ant-nodeps.jar")
    # File.join(".", "runcoderun", "tools.jar"),
    # File.join(".", "lib", "scala-library.jar")
  ].join(File::PATH_SEPARATOR)
  result = system "export JAVA_HOME=/usr/lib/jvm/java-6-sun && java -cp #{classpath} org.apache.tools.ant.Main -emacs test"
  puts "result of system java call: #{result}"
  result || abort
end


def system!(cmd)
  system(cmd) || abort
end