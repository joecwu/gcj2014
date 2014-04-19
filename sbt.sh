java -Xms512M -Xmx1536M -Xss256M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=768M -jar `dirname $0`/sbt-launch.jar "$@"
