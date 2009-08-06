# Ruby Rake file to allow automatic build testing on RunCodeRun.com

task :default => [:test]

task :test do
  classpath = [
    File.join(".", "runcoderun", "ant.jar"),
    File.join(".", "runcoderun", "ant-launcher.jar"),
    File.join(".", "runcoderun", "ant-nodeps.jar")
  ].join(File::PATH_SEPARATOR)
  exec "java -cp #{classpath} org.apache.tools.ant.Main -emacs build"
end
