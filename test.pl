add a new file

use strict;
use Data::Dumper;
use Date::Manip;
use FileHandle;
use POSIX;
use IO::File;
use XML::Simple;
use LWP::Simple;

our $debug = undef;

package TPRUN::Instance;
use Data::Dumper;
use Date::Manip;
use FileHandle;
use POSIX;
use IO::File;
use XML::Simple;
use LWP::Simple;
use LWP::UserAgent;
use File::Basename;
use Digest::SHA1  qw(sha1 sha1_hex sha1_base64);
use lib qw(/opt/TPsvn/lib);
use File::Path;

our $sleepForQuit = 2;

sub new {
  my ($class, $instancePropertiesFile) = @_;
  my $self = $class->parseConf($instancePropertiesFile);
  bless $self, $class;
  $self->metaDefaults();
  $self->blessSelf();
  $self->defaults();
  $self->resolveKeys();
  $self->setEnvironmentVariables();
  return $self; 
}

sub blessSelf {
  my $self = shift;
  if($self->{'rbcconfig.usemodule'} eq 'yes') {
    if($self->{'rbcconfig.binarytype'} eq 'java') {
      bless $self, 'TPRUN::Instance::ModuleJavaApp';
    } elsif ($self->{'rbcconfig.binarytype'} eq 'c') {
      bless $self, 'TPRUN::Instance::ModuleCApp';
    }
  } else {
    if($self->{'rbcconfig.binarytype'} eq 'c') {
      bless $self, 'TPRUN::Instance::GenericCApp';
    } elsif ($self->{'rbcconfig.binarytype'} eq 'java') {
      bless $self, 'TPRUN::Instance::GenericJavaApp';
    }
  }
}

sub setup {
  my ($self) = @_;
  $self->{'runtime'}->{'today'} = UnixDate(ParseDate("today"), "%Y-%m-%d");
  $self->writeControllerPidFile();
  $self->cleanUpLogs();
  $self->cleanUpVarDaily();
  $self->cleanUpVarWeekly();
  $self->setupLogger();
  $self->setupVarDaily();
  $self->setupVarWeekly();
  chdir($self->{'instance.home'});
}

sub killme {
  my $self = shift;
  if(defined($self->{'rbcconfig.stopclass'})) {
    $self->runStopClass();
  } 
        elsif (defined($self->{'rbcconfig.stopcommand'})) {
                $self->runStopCommand();
        }
  else {
    my $pid = $self->getAPid();
    kill SIGHUP, $pid;
  }
}

sub runStopCommand {
  my $self = shift;
  my $cmd = "bin/$self->{'rbcconfig.stopcommand'}";
  if(defined($self->{'rbcconfig.stopargs'})) { $cmd = "$cmd $self->{'rbcconfig.stopargs'}"; }
  print "Stopping application: $cmd\n";
  system $cmd;
}

sub runStopClass {
  my $self = shift;
  my $cmd = "$self->{'rbcconfig.javahome'}/bin/java $self->{'rbcconfig.stopclass'}";
  if(defined($self->{'rbcconfig.stopargs'})) { $cmd = "$cmd $self->{'rbcconfig.stopargs'}"; }
  print "Stopping application: $cmd\n";
  system $cmd;
}

sub getLogDir {
  my $self = shift;
  return defined($self->{'rbcconfig.logdir'}) ? $self->{'rbcconfig.logdir'} : "$self->{'instance.home'}/logs";
}

sub cleanUpPidFiles {
  my $self = shift;
  unlink $self->getCPidFile();
  unlink $self->getAPidFile();
}

sub cleanUpLogs {
  my $self = shift;
  if(defined($self->{'rbcconfig.logretention'})) {
    my $find = TPRUN::Util->getCommand("find");
    my $logdir = $self->getLogDir();
    my $cmd = "cd $logdir && $find . -name \"*.log\" -mtime +$self->{'rbcconfig.logretention'} -exec rm -f {} \\\;";
    system $cmd;
  }
}

sub cleanUpVarDaily {
  my $self = shift;
  if(defined($self->{'rbcconfig.dailyretention'})) {
    my $find = TPRUN::Util->getCommand("find");
    my $cmd = "cd $self->{'instance.home'}/var && $find . -name \"daily*\" -mtime +$self->{'rbcconfig.dailyretention'} -exec rm -rf {} \\\;";
    system $cmd;
  }
}

sub cleanUpVarWeekly {
  my $self = shift;
  if(defined($self->{'rbcconfig.weeklyretention'})) {
    my $find = TPRUN::Util->getCommand("find");
    my $cmd = "cd $self->{'instance.home'}/var && $find . -name \"weekly*\" -mtime +$self->{'rbcconfig.weeklyretention'} -exec rm -rf {} \\\;";
    system $cmd;
  }
}

sub setupVarWeekly {
  my $self = shift;
  my $monday = UnixDate(ParseDate("monday"), "%Y-%m-%d");
  $self->{'runtime'}->{'weekly'} = "$self->{'instance.home'}/var/weekly-$monday";
  system "mkdir -p $self->{'runtime'}->{'weekly'}";
  unlink("$self->{'instance.home'}/var/weekly");
  if(-d "$self->{'instance.home'}/var/weekly" or -f "$self->{'instance.home'}/var/weekly") {
    print STDERR "Can't create symlink $self->{'instance.home'}/var/weekly!!! Please remove $self->{'instance.home'}/var/weekly and try again!!!\n";
    exit(1);
  }
  symlink("weekly-$monday","$self->{'instance.home'}/var/weekly"); 
}

sub setupVarDaily {
  my $self = shift;
  $self->{'runtime'}->{'daily'} = "$self->{'instance.home'}/var/daily-$self->{'runtime'}->{'today'}";
  system "mkdir -p $self->{'runtime'}->{'daily'}";
  unlink("$self->{'instance.home'}/var/daily");
  if(-d "$self->{'instance.home'}/var/daily" or -f "$self->{'instance.home'}/var/daily") {
    print STDERR "Can't create symlink $self->{'instance.home'}/var/daily!!! Please remove $self->{'instance.home'}/var/daily and try again!!!\n";
    exit(1);
  }
  symlink("daily-$self->{'runtime'}->{'today'}", "$self->{'instance.home'}/var/daily");
}

sub setupLogger() {
  my $self = shift;

	return if (defined $self->{'runtime'}->{'console'});
	$self->{'runtime'}->{'today'} = UnixDate(ParseDate("today"), "%Y-%m-%d");

  my $logdir = $self->getLogDir();
  $self->{'runtime'}->{'daily'} = "$self->{'instance.home'}/var/daily-$self->{'runtime'}->{'today'}";
  $self->{'runtime'}->{'applogfile'} = "$logdir/$self->{'instance.name'}-$self->{'runtime'}->{'today'}.log";
  $self->{'runtime'}->{'controllerlogfile'} = $self->getTmpDir() . "/c.log";
  $self->{'runtime'}->{'ALOG'} = new FileHandle ">> $self->{'runtime'}->{'applogfile'}";
  die "Can't open $self->{'runtime'}->{'applogfile'} for write: $!" if(!defined($self->{'runtime'}->{'ALOG'}));
  $self->{'runtime'}->{'CLOG'} = new FileHandle "> $self->{'runtime'}->{'controllerlogfile'}";
  die "Can't open $self->{'runtime'}->{'controllerlogfile'} for write: $!" if(!defined($self->{'runtime'}->{'CLOG'}));
  unlink("$logdir/Daily");
	symlink("$self->{'instance.name'}-$self->{'runtime'}->{'today'}.log", "$logdir/Daily");
	
	#alarm when midnight
	alarm(TPRUN::Util->findTimeout());
}

sub writeControllerPidFile {
  my $self = shift;
  my $tmpdir = $self->getTmpDir();
  mkdir $tmpdir if(! -d $tmpdir); 
  die "Can't create $tmpdir: $!" if(! -d $tmpdir);
  TPRUN::Util->writeFile($self->getCPidFile(), $$);
  die "Can't write controller pid file: $!" if(TPRUN::Util->readFirstLine($self->getCPidFile()) ne $$);
}

sub writeApplicationPidFile {
  my ($self,$pid) = @_;
  my $tmpdir = $self->getTmpDir();
  mkdir $tmpdir if(! -d $tmpdir); 
  die "Can't create $tmpdir: $!" if(! -d $tmpdir);
  TPRUN::Util->writeFile($self->getAPidFile(), $pid);
  die "Can't write application pid file: $!" if(TPRUN::Util->readFirstLine($self->getAPidFile()) ne $pid);
}

sub getInstancePorpertiesFile {
  my $self = shift;
  return $self->{'instance.home'} . "/config/instance.properties";
}

# this method is only need in the constructor to determine what kind of object to return
sub metaDefaults {
  my $self = shift;
  # binary type
  $self->setIfNull('rbcconfig.binarytype', 'java');
  
  # use module or not
  $self->setIfNull('rbcconfig.usemodule', 'no');
}

sub defaults {
  my $self = shift;

  # repository directory
  $self->setIfNull('rbcconfig.repository.dir', 
	$self->{'instance.home'} . "/../repository");

  # config repository
  $self->setIfNull('rbcconfig.config.repo', 
	"http://tpeng-ax1-p.ny.rbcds.com:9000/svn/appconf");

  # deployment server
  $self->setIfNull('rbcconfig.deployserver.url',
        "http://tpeng-ax1-p.ny.rbcds.com:9000");

  $self->setIfNull('rbcconfig.command.svnadmin', "/home/tptools/Linux/x86_64/bin/svn"); 
  $self->setIfNull('rbcconfig.command.svn', "/home/tptools/Linux/x86_64/bin/svn"); 

  # svn stuff
  $self->setIfNull('rbcconfig.svn_create_url', 'http://tpeng-ax1-p.ny.rbcds.com:9000/cgi-bin/svn_create_repo.cgi');

  # default JRE
  $self->setIfNull('rbcconfig.javahome', '/tools/java/jre1.5.0_10');

  # default retry times
  $self->setIfNull('rbcconfig.retrytimes', 3);
  $self->{'originalRetrytimes'} = 3;

  # default rbcconfig.spawn
  $self->setIfNull('rbcconfig.spawn', 1);

  # default kill timeout
  $self->setIfNull('rbcconfig.killtimeout', 10);

  # default email address
  my $womami = TPRUN::Util->getCommand("whoami");
  my $email = `$womami`; chomp($email);
  $self->setIfNull('rbcconfig.email', $email);

  # lower case groupid and appname
  $self->{'rbcconfig.groupid'} = lc($self->{'rbcconfig.groupid'});
  $self->{'rbcconfig.appname'} = lc($self->{'rbcconfig.appname'});
}

sub setIfNull {
  my ($self, $key, $value) = @_;
  $self->{$key} = $value if(!defined($self->{$key}));
}

sub resolveKeys {
	my $self=shift;

	foreach my $key (keys %$self) {
		$self->{$key}=$self->resolveVal($key,$self->{$key});
	}
}

sub resolveVal {
	my $self=shift;
	my $primKey=shift;
        my $val=shift;
        my $recursionTracker=shift;

        if (!defined $recursionTracker) {
                my $tracker->{'keys'}={};
                $tracker->{'array'}=();
		$recursionTracker=\$tracker;
        }

        $$recursionTracker->{'keys'}->{$primKey}=1;
	push(@{$$recursionTracker->{'array'}},$primKey);
        while ($val =~ /#{(.+?)}/) {
                my $key = $1;
                if (defined $$recursionTracker->{'keys'}->{$key}) {
                        print STDERR "Circular dependence found at '$primKey' = '$val'\n";
			print STDERR "Printing trace:\n";
			foreach my $subKey (@{$$recursionTracker->{'array'}}) {
				print STDERR "$subKey\n";
			}
                        exit 3;
                }
                if (defined $self->{$key}) {
			my $subVal = $self->{$key};
			if ($subVal =~ /#{(.+?)}/) {
				$subVal = $self->resolveVal($key,$subVal,$recursionTracker);
			}
                        $val =~ s/#{$key}/$subVal/g;
                }
                else {
                        print STDERR "'$key' not found in properties file\n";
                        exit 3;
                }
        }

        if ($val =~ /`(.+?)`/) {
		my $toReplace = $1;
                my @result = `$toReplace`; chomp $result[0];
		$toReplace =~ s/\+/\\\+/g;
		$toReplace =~ s/\$/\\\$/g;
                $val =~ s/`$toReplace`/$result[0]/g;
        }

	$$recursionTracker=undef;
        return $val;
}

sub parseConfFiles {
  my $self = shift;
  my $result;
  foreach my $file (@_) {
    if(-f $file) {
      my $r = $self->parseConf($file);
      foreach my $key (keys %$r) {
        $result->{$key} = $r->{$key};
      }
    }
  }
  return $result;
}

sub parseConf {
  my ($self,$confFile) = @_;
  my $result;
  my $fh = new IO::File "< $confFile";
  die "Can't open $confFile for read: $!" if (!defined($fh));
  while(my $line = $fh->getline) {
    if($line =~ m/^\@include/) {
      my ($cmd, $file) = split(/\s+/, $line);
      my $hash = $self->parseConf($file);
      foreach my $key (keys %$hash) { $result->{$key} = $hash->{$key}; }
    } else {
      my ($key, $value) = $self->parseLine($line);
      if(defined($key) && defined($value)) { 
        $key =~ s/^\s+//; $value =~ s/^\s+//;
        $key =~ s/\s+$//; $value =~ s/\s+$//;
        $result->{$key} = $value; 
      }
    }
  }
  close $fh;
  return $result;
}

sub parseLine {
  my ($self, $line) = @_;
  return (undef,undef) if ($line =~ m/^#/ or $line =~ m/^$/);
  my $result;
  chomp($line);
  $line =~ s/^\s+//;
  $line =~ s/\s+$//;
  my ($key, @values) = split(/=/, $line);
  return (undef, undef) if ($#values == -1);
  my $value = join("=", @values);
  return ($key, $value);
}

sub getFile {
  my ($self, $groupId, $artifactId, $appversion, $type) = @_;
  return $self->{'rbcconfig.repository.dir'} . "/${groupId}/${type}s/${artifactId}-${appversion}.${type}";
}

sub getTmpDir {
  my ($self) = @_;
  return "/var/tmp/" . $self->{'instance.name'};
}

sub getCPidFile {
  my $self = shift;
  return $self->getTmpDir() . "/c.pid";
}

sub getAPidFile {
  my $self = shift;
  return $self->getTmpDir() . "/pid";
}

sub getCPid {
  my $self = shift;
  return TPRUN::Util->readFirstLine($self->getCPidFile());
}

sub getAPid {
  my $self = shift;
  return TPRUN::Util->readFirstLine($self->getAPidFile());
}

sub getControllerProcess {
  my $self = shift;
  return TPRUN::Util->getProcessByPID($self->getCPid());
}

sub getApplicationProcess {
  my $self = shift;
  return TPRUN::Util->getProcessByPID($self->getAPid());
}

sub getAppConfigVersionFilename {
  my $self = shift;
  return "$self->{'instance.home'}/.version";
}

sub getAppConfigVersion {
  my $filename = $_[0]->getAppConfigVersionFilename();
  return undef if(! -f $filename);
  return TPRUN::Util->readFirstLine($filename); 
}

sub status {
  my ($self, $vcs) = @_;
  my $status = 0;
  if(! -f $self->getCPidFile()) {
    print "Controller pid file not found, application is not running.\n";
    $status = 1;
  } else {
    my $cp = $self->getControllerProcess();
    my $ap = $self->getApplicationProcess();
    TPRUN::Util->showProcess($cp, "controller");
    TPRUN::Util->showProcess($ap, "application");
    $status = 1 if(!defined($cp) or !defined($ap));
    $self->showPerformance();
  }
  if($status == 0) { return defined($vcs) ? 110 : 0; }
  if($status == 1) { return defined($vcs) ? 100 : 1; }
}

sub coredump {
  my $self = shift;
  my $pid = $self->getAPid();
  if(defined($pid)) {
    if($self->{'rbcconfig.apptype'} eq 'java') {
      kill SIGQUIT, $pid;
    } else {
      kill SIGSEGV, $pid;
     }
  }
}

sub threadDump {
  kill SIGQUIT, shift->getAPid();
}

sub dumpHeap {
  my $cmd = "$ENV{'JAVA_HOME'}/bin/jmap -heap " . shift->getAPid();
  die "Can't find jmap under $ENV{'JAVA_HOME'}, you need JDK 1.6 and above" if(! -f "$ENV{'JAVA_HOME'}/bin/jmap");
  exec $cmd;
}

sub restartall {
  kill SIGUSR1, shift->getCPid();
}

sub restartapp {
  kill SIGUSR2, shift->getCPid();
}

sub disableretry {
  kill SIGTSTP, shift->getCPid();
}

sub enableretry {
  kill SIGCONT, shift->getCPid();
}

sub checkPids {
  my $self = shift;
  if( -f $self->getCPidFile()) {
    $self->log("Found a controller pid file, checking if the controller is up...\n");
    my $p = $self->getControllerProcess();
    if(defined($p)) {
      print("Controller already running!\n");
      exit 1;
    } else {
      $self->log("No process maps to " . $self->getCPid() . ", cleaning stale controller pid file\n");
      unlink($self->getCPidFile());
    }
  }
  if( -f $self->getAPidFile()) {
    my $p = $self->getApplicationProcess();
    if(defined($p)) {
      print("Application already running!\n");
      exit 1;
    } else {
      $self->log("No process maps to " . $self->getAPid() . ", cleaning stale application pid file\n");
      unlink($self->getAPidFile());
    }
  }
}

sub log {
  my ($self, @lines) = @_;
  my $fh = $self->{'runtime'}->{'CLOG'};
  my $fh1 = $self->{'runtime'}->{'ALOG'};
  my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
  if(defined($fh) && defined($fh1)) {
	select((select($fh), $|=1)[0]);
	select((select($fh1), $|=1)[0]);
    printf $fh "%4d-%02d-%02d %02d:%02d:%02d\t",$year+1900,$mon+1,$mday,$hour,$min,$sec ;
    printf $fh1 "%4d-%02d-%02d %02d:%02d:%02d\t",$year+1900,$mon+1,$mday,$hour,$min,$sec ;
    print $fh @lines;
    print $fh1 @lines;
  } else {
    print @lines;
  }
}

sub respawn {
  my $self = shift;
  my $proc = TPRUN::Util->getProcessByPID($$);
  my $str = '-app ' . $self->{'instance.name'};
  if($proc->cmndline =~ /$str/) {} else {
    my $cmd = $0 . " -app " . $self->{'instance.name'};
    foreach (@ARGV) {
      $cmd = $cmd . " $_";
    }
    if(!defined($self->{'runtime'}->{'daemon'})) {print("need to respawn $cmd\n");}
    exec("ulimit -S -c unlimited;exec $cmd");
  }
}

sub setEnvironmentVariables {
  my $self = shift;
  foreach my $key (%$self) {
    if($key =~ /^rbcconfig\.env_file/) {
      $self->setUpEnvironmentFromFile($self->{$key});
    } elsif($key =~ /^rbcconfig\.env\./) {
      my (undef, undef, @vars) = split(/\./, $key);
      my $var = join('.', @vars);
      $ENV{$var} = $self->{$key};
    } 
  }
  $self->setUpLocalLib();
}

sub setUpEnvironmentFromFile {
  my ($self,$file) = @_;
  #$self->log("setup Environment from $file ...\n");
  open(INFILE, "< $file") or die "Can't open $file : $!\n";
  while(<INFILE>) {
    next if (m/^#/);
    my ($key, $value) = split(/=/);
    if(defined($key) && defined($value)) {
      $key =~ s/^\s+//; $key =~ s/\s+$//;
      $value =~ s/^\s+//; $value =~ s/\s+$//;
      $ENV{$key} = $value;
    }
  } 
  close INFILE;
}

sub setUpLocalLib {
  my $self = shift;
  my $find = TPRUN::Util->getCommand("find");
  my $localLibDir = "$self->{'instance.home'}/lib";
  my @libdirs;
  @libdirs = `$find $localLibDir -type d -print 2> /dev/null` if(-d $localLibDir);
  my $addondir = "";
  foreach my $libdir (@libdirs) {
    chomp($libdir);
    $addondir .= ":${libdir}";
  }
  if(defined($ENV{'LD_LIBRARY_PATH'})) {
    $ENV{'LD_LIBRARY_PATH'} = $addondir . ':' . $ENV{'LD_LIBRARY_PATH'};
  } else {
    $ENV{'LD_LIBRARY_PATH'} = $addondir;
  }
}

sub run {
  my $self = shift;
  print STDOUT "Starting $self->{'instance.name'}...\n";

	if (!defined $self->{'runtime'}->{'console'}) {
		$self->setupOutput();
	}
}

sub setupOutput {
	my $self=shift;

	open STDOUT, ">> " . $self->{'runtime'}->{'applogfile'} or die $!;
	open STDERR, ">&STDOUT" or die $!;
}

sub showEnvironment {
	my $self = shift;
	$self->log(`env | sort`);
}

sub killall {
  my ($self, $forcestop) = @_;
  if(! -f $self->getCPidFile()) {
    print STDERR $self->getCPidFile() . " does not exist. Either the application is not running, or you have to manually kill it.\n";
    exit(1);
  }
  my $controller = $self->getControllerProcess();
  if(!defined($controller)) {
    print STDERR $self->getCPidFile() . " does not exist. Either the application is not running, or you have to manually kill it.\n";
    exit(1);
  }
  if(defined($self->{'rbcconfig.nostopbefore'}) and !defined($forcestop)) {
    if($self->{'rbcconfig.nostopbefore'} =~ m/^\d\d:\d\d/) {
      my $bytime = ParseDate($self->{'rbcconfig.nostopbefore'});
      if(Date_Cmp($bytime, ParseDate("now")) > 0) {
        print STDERR "Can't stop process after rbcconfig.nostopbefore=$self->{'rbcconfig.nostopbefore'}\nUse $0 -forcestop instead\n";
        exit(1); 
      }
    } else {
      print STDERR "rbcconfig.nostopbefore should be in the format of hh:mm\n";
      exit(1);
    }
  }
  my $pid = $controller->pid;
  if(!defined($controller)) { print "Application is not running.\n"; exit(0); }
  kill SIGHUP, $pid;
  my $n = 0;
  my $content;
  while(-f $self->getCPidFile()) {
    $n++;
    sleep($sleepForQuit/2);
    if($n == $self->{'rbcconfig.killtimeout'}) {
      if(!defined($self->{'rbcconfig.donotkill9'}) or $self->{'rbcconfig.donotkill9'} eq 'no') {
        my $apid = $self->getAPid();
        $content = "Can not terminate process, sending kill -9 to $pid (controller)\n";
        $content .= "Can not terminate process, sending kill -9 to $apid (application)\n";
        $content .= "Please look into the problem!!!\n";
        kill SIGKILL, $apid;
        kill SIGKILL, $pid;
      } else {
        $content = "Can not terminate process and donotkill9 is defined, you need to manually kill the process!!!\n";
      }
      TPRUN::Util->email($self->{'rbcconfig.email'}, "TPRUN can't kill " . $self->{'instance.name'} . " !!!", $content);
      #&email($self,"TPRUN can't kill $name !!!",$content);
      print STDERR $content;
      exit(1);
    }
  }
  print "Process killed.\n";
}

sub showAppVersion {
  my $self = shift;
  my $version = $self->getAppConfigVersion();
  print "Binary Version: $self->{'rbcconfig.appversion'}\n";
  if(!defined($version)) {
    print "Config Version: Never Checked In\n";
  } else {
    print "Config Version: " . $version . "\n";
  }
}

sub checkFingerPrints {
  my ($self) = @_;
  if(! -f "$self->{'instance.home'}/.version") {
    TPRUN::Util->email($self->{'rbcconfig.email'}, "$self->{'instance.name'} has never been checked into SVN repository","Please run tprun -checkin -user <user> -comments <comments> to check in configurations");
    return undef;
  }
  my $text = $self->svn_diff();
  if(defined($text) && $text ne "") {
    TPRUN::Util->email($self->{'rbcconfig.email'}, "$self->{'instance.name'} has not been checked into SVN repository", $text);
  }
}

sub notCheckedInYet {
  my $self = shift;
  TPRUN::Util->email($self->{'rbcconfig.email'}, "Checksum mismatch for $self->{'instance.name'} on $self->{'instance.host'}",
      "Finger print mismatchf for $self->{'instance.name'} on $self->{'instance.host'}.");
}

sub setUpForAdmin {
  my $self = shift;
  $ENV{'JAVA_HOME'} = $self->{'rbcconfig.javahome'};
  if(ref($self) eq 'TPRUN::Instance::ModuleJavaApp' or ref($self) eq 'TPRUN::Instance::GenericJavaApp') {
    $ENV{'CLASSPATH'} = $self->getClassPath();
  } else {
    $ENV{'CLASSPATH'} = $self->getLocalClassPath();  
  }
}

sub getLocalClassPath {
  my $self = shift;
  my $classpath = '';
  $classpath = $ENV{'CLASSPATH'} if (defined($ENV{'CLASSPATH'}));
  my $ls = TPRUN::Util->getCommand("ls");
  foreach my $jar (`$ls $self->{'instance.home'}/jars/*.jar 2>/dev/null`) {
    chomp($jar);
    $classpath .= TPRUN::Util->getClassPathSeparator() . $jar;
  }
  return $classpath;
}


sub admin {
  my ($self) = @_;
  if(defined($self->{'instance.admin.type'}) && $self->{'instance.admin.type'} eq 'qcon') {
    $self->adminByQcon("$self->{'instance.host'}:$self->{'instance.admin.port'}");
  } else {
    $self->adminByURL("$self->{'instance.host'}:$self->{'instance.admin.port'}");
  }
}

sub adminByQcon {
  my ($self, $url) = @_;
  my $rlwrap = TPRUN::Util->getCommand('rlwrap');
  my $cmd = "$rlwrap $ENV{'QHOME'}/l64/qcon $url";
  print "$cmd\n";
  exec($cmd); 
}

sub adminByURL {
  my ($self, $url) = @_;
  $self->setUpForAdmin();
  my $cmd;
  if(defined($self->{'instance.admin.type'}) && $self->{'instance.admin.type'} eq 'qcon') {
    my $rlwrap = TPRUN::Util->getCommand('rlwrap');
    $cmd = "$rlwrap $ENV{'QHOME'}/l64/qcon $url";
  } else {
    $cmd = "$self->{'rbcconfig.javahome'}/bin/java $url";
  }
  exec($cmd);
}

sub adminCmd {
  my ($self, $adminCmd) = @_;
  $self->adminCmdByURL("$self->{'instance.host'}:$self->{'instance.admin.port'}", $adminCmd);
}

sub adminCmdByURL {
  my ($self, $url, $adminCmd) = @_;
  $self->setUpForAdmin();
  my $cmd;
  if(defined($self->{'instance.admin.type'}) && $self->{'instance.admin.type'} eq 'qcon') {
    $cmd = "(echo \"$adminCmd\"; echo \"\\\\\\\\\") |$ENV{'QHOME'}/l64/qcon $url";
  } else {
    $cmd = "$self->{'rbcconfig.javahome'}/bin/java $url $adminCmd";
  }
  print "$cmd\n";
  exec($cmd);
}

# this is a class method (factory) returns an instance
sub create {
  my $class = shift;
  my $create_file = shift;
  my ($deployDir);

  die "TPRUN::Instance::create is a class method" if(ref($class));
  my $self = {};

	if ($create_file) {
		$deployDir = `pwd`; chomp $deployDir;
		$self = $class->parseConf($create_file);
    		$self->{'instance.name'} = TPRUN::Util->ask("Name of the instance?");
    		$self->{'instance.home'} = "${deployDir}/$self->{'instance.name'}";
	}	

  bless $self, $class;
  $self->metaDefaults();

  if (!defined $create_file) {
    $self->{'rbcconfig.binarytype'} = TPRUN::Util->ask("What type of application you are installing?", "c", "java");
    $self->{'rbcconfig.groupid'} = TPRUN::Util->ask("What's groupid (ex. efg, accel)?");
    $self->{'rbcconfig.appname'} = TPRUN::Util->ask("What's appname (ex. fd, dma)?");
    $self->{'rbcconfig.appversion'} = TPRUN::Util->ask("What's appversion?");
    $deployDir = TPRUN::Util->ask("Directory of the installation?");
        if ($deployDir eq '.') {
                $deployDir = `pwd`; chomp $deployDir;
        }
    while(! -d $deployDir) {
       print STDERR "${deployDir} does not exist, please create the directory then hit any key to proceed\n";
       my $dum = <STDIN>;
    }
    $self->{'instance.name'} = TPRUN::Util->ask("Name of the instance?");
    $self->{'instance.home'} = "${deployDir}/$self->{'instance.name'}";
    $self->{'rbcconfig.usemodule'} = TPRUN::Util->ask("Is it module based (choose no if it is a tar ball)?","yes", "no");
    if($self->{'rbcconfig.usemodule'} ne 'yes') {
      if(TPRUN::Util->ask("Is your application available on the deployment server?", "yes", "no") ne "yes") {
        print "Which directory is your application ($self->{'rbcconfig.appname'}-$self->{'rbcconfig.appversion'}.tar.gz) located? ";
        $self->{'rbcconfig.runtime.localfile'} = <STDIN>; chomp($self->{'rbcconfig.runtime.localfile'});
        $self->{'rbcconfig.runtime.localfile'} .= "/$self->{'rbcconfig.appname'}-$self->{'rbcconfig.appversion'}.tar.gz";
      }
    }
  }
  $self->blessSelf();
  $self->defaults();
	$self->resolveKeys();
  $self->createInstance();
  $self->deployTPRUN();
    $self->deploy();
#  $self->svn("create");
#  $self->svn("import");
}

sub clone {
  my $class = shift;
  die "TPRUN::Instance::clone is a class method" if(ref($class));
}

sub checkin {
  my ($self,$user,$comments) = @_;
  if(!defined($user) or !defined($comments)) {
    print "You need to specify both --user and --comments in order to checkin\n";
    exit(1);
  }
  $self->svn("checkin", $user, $comments);
}

sub checkout {
  my ($self, $version) = @_;
  $self->svn("checkout", $version);
}

sub svn {
  my ($self,$cmd,@args) = @_;
  if($cmd eq "checkin") {
    $self->svn_checkin(@args);
    return;
  }
  if($cmd eq "checkout") {
    $self->svn_checkout(@args);
    return;
  }
  if($cmd eq "import") {
    $self->svn_import();
    return;
  }
  if($cmd eq "list") {
    TPRUN::SVN->list_tags($self);
    return;
  }
  if($cmd eq "diff") {
    print $self->svn_diff(@args);
    return;
  }
  my $oscmd = "cd $self->{'instance.home'}/config; $self->{'rbcconfig.command.svn'} $cmd " . join(" ", @args) . " ; cd $self->{'instance.home'}";
  print $oscmd, "\n";
  system $oscmd;
}

sub svn_get_repo_root {
  my $self = shift;
  return "$self->{'rbcconfig.config.repo'}/" . $self->svn_get_tmp_dir_name();
}

sub svn_get_trunk_url {
  my $self = shift;
  return $self->svn_get_repo_url() . "/trunk";
}

sub svn_get_tags_url {
  my $self = shift;
  return $self->svn_get_repo_url() . "/tags";
}

sub svn_get_branches_url {
  my $self = shift;
  return $self->svn_get_repo_url() . "/branches";
}

sub svn_get_tmp_dir_name {
  my $self = shift;
  return ".$self->{'instance.name'}";
}

sub svn_get_tmp_dir {
  my $self = shift;
  return $self->{'instance.home'} . "/" . $self->svn_get_tmp_dir_name();
}

sub svn_get_version_file {
  my $self = shift;
  return $self->{'instance.home'} . "/.version";
}

sub svn_get_tmp_trunk_dir {
  my $self = shift;
  return $self->svn_get_tmp_dir() . "/trunk";
}

sub svn_get_tmp_branches_dir {
  my $self = shift;
  return $self->svn_get_tmp_dir() . "/branches";
}

sub svn_get_tmp_branch_dir {
  my ($self, $appversion) = @_;
  return $self->svn_get_tmp_branches_dir() . "/" . $appversion;
}

sub svn_get_tmp_tags_dir {
  my $self = shift;
  return $self->svn_get_tmp_dir() . "/tags";
}

sub svn_get_tmp_tag_dir {
  my ($self, $version) = @_;
  return $self->svn_get_tmp_tags_dir() . "/" . $version;
}

sub svn_get_repo_url {
  my $self = shift;
  return $self->svn_get_repo_root();
}

sub svn_version {
  my ($self, $version) = @_;
  if(defined($version)) {
    TPRUN::Util->writeFile($self->svn_get_version_file(), $version); 
  } 
  $version = TPRUN::Util->readFirstLine($self->svn_get_version_file()); 
  return $version;
}

sub svn_import {
  my $self = shift;
  $self->cleanUpSVNDirFromConfig();
  TPRUN::SVN->createRepository($self);
  TPRUN::SVN->prepare($self);
  TPRUN::SVN->overwriteTrunk($self);
  TPRUN::SVN->importTrunk($self);
  #TPRUN::SVN->createBranchFromTrunk($self, "binary-" . $self->{'rbcconfig.appversion'}, "initial branch for $self->{'rbcconfig.appversion'}");
  TPRUN::SVN->createBranchFromTrunk($self, $self->{'rbcconfig.appversion'}, "initial branch for $self->{'rbcconfig.appversion'}");
  #TPRUN::SVN->createTag($self, "binary-" . $self->{'rbcconfig.appversion'}, "config-" . $self->{'rbcconfig.appversion'} . ".0", "initial tag for $self->{'rbcconfig.appversion'}");
  TPRUN::SVN->createTag($self, $self->{'rbcconfig.appversion'}, $self->{'rbcconfig.appversion'} . "-0", "initial tag for $self->{'rbcconfig.appversion'}");
  $self->svn_version($self->{'rbcconfig.appversion'} . "-0");
  $self->svn('list');
}

sub cleanUpSVNDirFromConfig {
  my $self = shift;
  my $find = TPRUN::Util->getCommand("find");
  my $cmd = "$find $self->{'instance.home'}/config -name \".svn\" -exec rm -rf {} \\; 2> /dev/null";
  TPRUN::Util->runOSCommand($cmd);
}

sub save_current_config {
  my $self = shift;
  my $cmd = TPRUN::Util->getCommand("rm") . " -rf $self->{'instance.home'}/.config.old";
  print "$cmd\n";
  system $cmd;
  $cmd = TPRUN::Util->getCommand("mv") . " $self->{'instance.home'}/config $self->{'instance.home'}/.config.old";
  print "$cmd\n";
  system $cmd;
}

sub svn_checkin {
  my ($self, $user, $comments) = @_;
  TPRUN::SVN->prepare($self);
  my $final_comments = "${comments} by ${user} for version " . $self->{'rbcconfig.appversion'};
  my $trunk_url = $self->svn_get_repo_url() . "/trunk";
  if(TPRUN::SVN->ifExists($self, $trunk_url) eq "NO") {
    print "Can not find $self->{'instance.name'} in SVN repository. Please run 'tprun -import' to create $self->{'instance.name'} in SVN repository\n";
    return;
  }
  my $branch_url = $self->svn_get_repo_url() . "/branches/$self->{'rbcconfig.appversion'}";
  if(TPRUN::SVN->ifExists($self,$branch_url) eq "NO") {
    print "First time checkin for binary version $self->{'rbcconfig.appversion'}\n";
    $self->svn_checkin_for_new_binary($final_comments);
  } else {
    $self->svn_checkin_for_old_binary($final_comments);
  }
}

sub svn_diff {
  my ($self, $version) = @_;
  my $trunk_url = $self->svn_get_repo_url() . "/trunk";
  if(TPRUN::SVN->ifExists($self, $trunk_url) eq "NO") {
    print STDERR "Can not find $self->{'instance.name'} in SVN repository. Please run 'tprun -import' to create $self->{'instance.name'} in SVN repository\n";
    return;
  }
  my $branch_url = $self->svn_get_repo_url() . "/branches/$self->{'rbcconfig.appversion'}";
  TPRUN::SVN->prepare($self);
  if(TPRUN::SVN->ifExists($self,$branch_url) eq "NO") {
    print STDERR "You have not checked in any configuration for binary version $self->{'rbcconfig.appversion'}. Please check in your configuration before diffing.\n";
    return;
  } else {
    TPRUN::SVN->checkoutBranch($self, $self->{'rbcconfig.appversion'});
    TPRUN::SVN->cleanUpFiles($self, $self->svn_get_tmp_branch_dir($self->{'rbcconfig.appversion'}));
    TPRUN::SVN->overwriteBranch($self, $self->{'rbcconfig.appversion'});
    TPRUN::SVN->prepareNewFiles($self, $self->svn_get_tmp_branch_dir($self->{'rbcconfig.appversion'}));
    my $tag = $self->getAppConfigVersion();
    my $release = TPRUN::SVN->getReleaseByTag($self, $tag);
    return join("",TPRUN::SVN->diffBranch($self, $release));
  }
}

sub svn_checkin_for_new_binary {
  my ($self, $comments) = @_;
  TPRUN::SVN->checkoutTrunk($self);
  TPRUN::SVN->cleanUpFiles($self, $self->svn_get_tmp_trunk_dir());
  TPRUN::SVN->overwriteTrunk($self);
  TPRUN::SVN->prepareNewFiles($self, $self->svn_get_tmp_trunk_dir());
  TPRUN::SVN->commitTrunk($self,$comments);
  TPRUN::SVN->createBranchFromTrunk($self, $self->{'rbcconfig.appversion'}, $comments);
  TPRUN::SVN->createTag($self, $self->{'rbcconfig.appversion'}, $self->{'rbcconfig.appversion'} .  "-0", $comments);
  $self->svn_version($self->{'rbcconfig.appversion'} .  "-0");
}

sub svn_checkin_for_old_binary {
  my ($self, $comments) = @_;
  TPRUN::SVN->checkoutBranch($self, $self->{'rbcconfig.appversion'});
  TPRUN::SVN->cleanUpFiles($self, $self->svn_get_tmp_branch_dir($self->{'rbcconfig.appversion'}));
  TPRUN::SVN->overwriteBranch($self, $self->{'rbcconfig.appversion'});
  TPRUN::SVN->prepareNewFiles($self, $self->svn_get_tmp_branch_dir($self->{'rbcconfig.appversion'}));
  my $tag = $self->getAppConfigVersion();
  my $release = TPRUN::SVN->getReleaseByTag($self, $tag);
  my @lines = TPRUN::SVN->diffBranch($self,$release); chomp(@lines);
  if(defined($lines[0]) && $lines[0] ne '') {
    TPRUN::SVN->commitBranch($self, $self->{'rbcconfig.appversion'}, $comments);
    my $nextTag = TPRUN::SVN->getNextTag($self, $self->{'rbcconfig.appversion'});
    TPRUN::SVN->createTag($self, $self->{'rbcconfig.appversion'}, "$self->{'rbcconfig.appversion'}-${nextTag}", $comments);
    $self->svn_version("$self->{'rbcconfig.appversion'}-${nextTag}");
  } else {
    print "No difference to checkin\n";
  }
}

sub svn_checkout {
  my ($self, $version) = @_;
  TPRUN::SVN->prepare($self);
  TPRUN::SVN->checkoutTag($self,$version);
  TPRUN::SVN->overwriteConfigFromTag($self, $version);
  $self->svn_version($version);
}

sub deployTPRUN {
  my $self = shift;
  my $tprunurl = $self->{'rbcconfig.tprun.url'};
  print "Getting tprun from $tprunurl ...";
  my $tprunContent = get $tprunurl;
  my $tprun = $self->getTPRUNFilename;

  if (!defined($tprunContent)) {
    print STDERR "Can't get tprun from ${tprunurl}: $!...copying from running copy\n";
    my $cmd = "cp -p $0 $tprun";
    my $result = system $cmd;
	die "Could not copy tprun from running copy => $result\n" if $result;
  }
  else {
    TPRUN::Util->writeFile($tprun, $tprunContent);
    chmod 0755, $tprun;
  }
  print "done.\n";
}

sub showPerformance {
  my $self = shift;
  my $result = TPRUN::Util->getProcessStatus($self->getAPid(),$self->getCPid());
  if(defined($result)) {
    print "time=$result->{'time'}; instance=$self->{'instance.name'}; cpu=$result->{'cpu'}; nThreads=$result->{'threads'}; rss=$result->{'rss'}; vsize=$result->{'vsize'}\n";
  } else {
   print "Application not running.\n";
  }
}

sub createInstance {
  my $self = shift;
  if(-d $self->{'instance.home'}) {
    print "$self->{'instance.home'} already exists, do you want to continue? (y|n) "
	. "($self->{'instance.home'} will be deleted and recreated by the installation)";
    my $answer = <STDIN>; chomp($answer);
    exit(1) if($answer ne "yes" && $answer ne "y");
    print "Are you sure? (y|n) ";
    $answer = <STDIN>; chomp($answer);
    exit(1) if($answer ne "yes" && $answer ne "y");
    print "Remove old installation from $self->{'instance.home'}..";
    system "rm -rf $self->{'instance.home'}/*";
    print "done\n";
  }
  my $logdir = $self->getLogDir();
  mkdir($self->{'instance.home'}, 0755);
  mkdir("$self->{'instance.home'}/scripts", 0755);
  mkdir("$self->{'instance.home'}/config", 0755);
  mkdir("$self->{'instance.home'}/scripts", 0755);
  mkdir("$self->{'instance.home'}/var", 0755);
  mkdir($logdir, 0755);
  mkdir("$self->{'instance.home'}/bin", 0755);
  mkdir("$self->{'instance.home'}/lib", 0755);
  mkdir("$self->{'instance.home'}/jars", 0755);
  open(CONF, "> $self->{'instance.home'}/config/instance.properties") || die "Can't open $self->{'instance.home'}/config/instance.properties'} for write: $!\n";
  foreach my $key (sort keys %$self) {
    next if (($key !~ /^instance/ and $key !~ /^rbcconfig/) or $key =~ /^rbcconfig.runtime/);
    print CONF "${key}=$self->{$key}\n";
  }
  close(CONF);
}

sub getTPRUNFilename {
  my $self = shift;
  return "$self->{'instance.home'}/scripts/tprun";
}

sub getClassPath {
  my $self = shift;
  my $classpath = '';
  $classpath = $ENV{'CLASSPATH'} if (defined($ENV{'CLASSPATH'}));
  my $ls = TPRUN::Util->getCommand("ls");
  foreach my $jar (`$ls $self->{'instance.home'}/jars/*.jar 2>/dev/null`) {
    chomp($jar);
    $classpath .= TPRUN::Util->getClassPathSeparator() . $jar; 
  }
  return $classpath;
}

1;

package TPRUN::Instance::GenericCApp;

require Exporter;
use LWP::UserAgent;
use LWP::Simple;
use File::Basename;

our @ISA = qw(Exporter TPRUN::Instance TPRUN::Instance::ModuleCApp);

#sub installStaticConfigurationsFromMainJar {
#}

sub defaults {
  my $self = shift;
  $self->SUPER::defaults();
}
#
sub run {
  my $self = shift;
	$ENV{'CLASSPATH'} = $self->getClassPath();
  $self->SUPER::run();
  #$self->setEnvironmentVariables();  
  my $cmd = "$self->{'instance.home'}/bin/$self->{'rbcconfig.maincode'}";
  $cmd .= " $self->{'rbcconfig.args'}" if(defined($self->{'rbcconfig.args'}));
  $self->log("######################## Environment Variables: \n"); 
  $self->log(`env`);
  $self->log("$cmd\n");
  exec($cmd);
}

1;

package TPRUN::Instance::GenericJavaApp;

require Exporter;
use File::Basename;
use LWP::Simple;
use LWP::UserAgent;

our @ISA = qw(Exporter TPRUN::Instance TPRUN::Instance::ModuleCApp TPRUN::Instance::JavaApp);

sub installStaticConfigurationsFromMainJar {
}

sub defaults {
  my $self = shift;
  $self->SUPER::defaults();
  # default JRE parameters
  $self->setIfNull('rbcconfig.args', '-Xnoclassgc -server -Xms1024m -Xmx2048m');

  # if java debug is enabled
  if(defined($ENV{'JAVA_DEBUG_PORT'})) {
    $self->{'rbcconfig.args'} .= " -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=$ENV{JAVA_DEBUG_PORT}";
  }

}

sub run {
  my $self = shift;
  $self->SUPER::run();
  #$self->setEnvironmentVariables();
  $ENV{'JAVA_HOME'} = $self->{'rbcconfig.javahome'};
  $ENV{'CLASSPATH'} = $self->getClassPath();

  my $cmd = "$self->{'rbcconfig.javahome'}/bin/java -DConfigFileName=$self->{'instance.home'}/config/env.xml -DAppProperties=$self->{'instance.home'}/config/instance.properties ";
  $cmd .= $self->{'rbcconfig.args'} if(defined($self->{'rbcconfig.args'}));
  $cmd .= " " . $self->{'rbcconfig.mainclass'};
  $cmd .= " " . $self->{'rbcconfig.args1'} if (defined($self->{'rbcconfig.args1'}));
  $self->log(`env`);
  $self->log("$cmd\n");
  exec($cmd);
}


1;

package TPRUN::Instance::JavaApp;

require Exporter;
use File::Basename;
use LWP::Simple;
use LWP::UserAgent;

our @ISA = qw(Exporter TPRUN::Instance);

sub defaults {
  my $self = shift;
  $self->SUPER::defaults();

  # default JRE parameters
  $self->setIfNull('rbcconfig.args', '-Xnoclassgc -server -Xms1024m -Xmx2048m');

  # if java debug is enabled
  if(defined($ENV{'JAVA_DEBUG_PORT'})) {
    $self->{'rbcconfig.args'} .= " -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=$ENV{JAVA_DEBUG_PORT}";
  }
}



1;

package TPRUN::Instance::ModuleCApp;

use Data::Dumper;
use Date::Manip;
use FileHandle;
use POSIX;
use IO::File;
use XML::Simple;
use LWP::Simple;
use LWP::UserAgent;
use File::Basename;

require Exporter;

our @ISA = qw(Exporter TPRUN::Instance);

sub installBinary {
  my ($self) = @_;
  my $binaryDir = TPRUN::Util->getCanonicalFilename($self->getBinaryDir());  
  my $home = TPRUN::Util->getCanonicalFilename($self->{'instance.home'});
  my $TAR = TPRUN::Util->getCommand("tar");
  if(-d $binaryDir) {
    my $cmd = "cd $binaryDir; tar cvf - . | (cd $home; tar xvf -)";
    print "$cmd\n";
    system "$cmd";
  } else {
    print STDERR "Can't find ${binaryDir}, please check the tar ball structure\n";
    exit(1);
  }
}

sub deploy {
  my $self = shift;
  $self->deployBinaries();
  $self->cleanUpBinaries();
  $self->installBinary();
}

sub deployBinaries {
  my ($self) = @_;
	if(defined $self->{'rbcconfig.localdeploydir'}) {
		my $file_name = "$self->{'rbcconfig.appname'}-$self->{'rbcconfig.appversion'}.tar.gz";
		$self->{'rbcconfig.runtime.localfile'} = $self->{'rbcconfig.localdeploydir'} . "/$file_name";
	}
  if($self->isDeployed()) {
    print $self->getBinaryFile . " is available on local server.\n";
  } else {
    $self->downloadBinary();
  }
}

sub cleanUpBinaries {
  my ($self) = @_;
  my $rm = TPRUN::Util->getCommand("rm");
  my $cmd = "$rm -rf $self->{'instance.home'}/bin/*";
  system $cmd;
  $cmd = "$rm -rf $self->{'instance.home'}/lib/*";
  system $cmd;
  $cmd = "$rm -rf $self->{'instance.home'}/jars/*";
  system $cmd;
}

sub downloadBinary {
  my $self = shift;
  if(defined($self->{'rbcconfig.runtime.localfile'})) {
    my $dir = dirname($self->getBinaryFile());
    TPRUN::Util->createDir($dir);
    my $cmd = "cp -f $self->{'rbcconfig.runtime.localfile'} " . $self->getBinaryFile();
    print "$cmd\n";
    system "$cmd";
  } else {
    my $file = $self->getRemoteBinaryFile();
    print "getting " . $file . " ...";
    my $binFileContent = get $file;
    die $file . " not available or size 0, exiting...\n" if(!defined($binFileContent));
    print "done\n";
    TPRUN::Util->writeFile($self->getBinaryFile(),$binFileContent);
  }
  my $binaryFile = TPRUN::Util->getCanonicalFilename($self->getBinaryFile());
  my $cmd = "cd " . $self->getBinaryRoot() . '; ' . TPRUN::Util->getCommand("gunzip") . "< "
	. $binaryFile . ' | ' . TPRUN::Util->getCommand("tar") . ' xvf - ';
  print "$cmd\n";
  system "$cmd"; 
}


sub isDeployed {
  my ($self) = @_;
  return undef if(! -f $self->getBinaryFile() or ! -d $self->getBinaryDir());
  return 1;
}

sub run {
  my $self = shift;
	$ENV{'CLASSPATH'} = $self->getClassPath();
  $self->SUPER::run();
  #$self->setEnvironmentVariables();  
  my $cmd = "$self->{'instance.home'}/bin/$self->{'rbcconfig.maincode'}";
  $cmd .= " $self->{'rbcconfig.args'}" if(defined($self->{'rbcconfig.args'}));
  $self->log(`env`);
  $self->log("$cmd\n");
  exec($cmd);
}

sub getBinaryRoot {
  my ($self) = @_;
  return "$self->{'rbcconfig.repository.dir'}/$self->{'rbcconfig.groupid'}/binaries/$self->{'rbcconfig.appname'}";
}

sub getBinaryFile {
  my ($self) = @_;
  return $self->getBinaryRoot() . "/$self->{'rbcconfig.appname'}-$self->{'rbcconfig.appversion'}.tar.gz";
}

sub getBinaryDir {
  my ($self) = @_;
  return $self->getBinaryRoot() . "/$self->{'rbcconfig.appname'}-$self->{'rbcconfig.appversion'}";
}

sub getRemoteBinaryFile {
  my ($self) = @_;
  return "$self->{'rbcconfig.runtime.localfile'}" if defined($self->{'rbcconfig.runtime.localfile'});
  return "$self->{'rbcconfig.deployserver.url'}/binaries/$self->{'rbcconfig.groupid'}/$self->{'rbcconfig.appname'}-$self->{'rbcconfig.appversion'}.tar.gz";
}

1;

package TPRUN::Instance::ModuleJavaApp;
use strict;
use Data::Dumper;
use IO::File;
use File::Basename;
use XML::Simple;
use LWP::Simple;
use LWP::UserAgent;

require Exporter;

our @ISA = qw(Exporter TPRUN::Instance::JavaApp);

sub installStaticConfigurationsFromMainJar {
  my ($self) = @_;
  my $mainJar = TPRUN::Util->getCanonicalFilename($self->getMainJarFile());
  my $home = $self->{'instance.home'};
  my $UNZIP = "/usr/bin/unzip";
  my $TAR = "/bin/tar";
  mkdir "/tmp/$$";
  print "Extracting Static Configuration files from " . $self->getMainJarFile() . "...\n";
  system "cd /tmp/$$; $UNZIP $mainJar > /dev/null";
  if(-d "/tmp/$$/deploy") {
    system "cd /tmp/$$/deploy; $TAR cvf - . | (cd $home; $TAR xvf -) > /dev/null";
  }
  system "rm -rf /tmp/$$";
}

sub deploy {
  my $self = shift;
  $self->deployBinaries();
  $self->installStaticConfigurationsFromMainJar();
}

sub deployBinaries {
  my ($self) = @_;
  if($self->isDeployed) {
    print $self->getMainJarFile . " already deployed under ". $self->{'rbcconfig.repository.dir'} . "\n";
    return;
  } else {
    print "Can't find " . $self->getMainJarFile . " under ". $self->{'rbcconfig.repository.dir'} . " deploying...\n";
  }
  $self->deployOneComponent($self->{'rbcconfig.groupid'}, $self->{'rbcconfig.appname'}, $self->{'rbcconfig.appversion'});
  foreach my $dep ($self->getDependencies()) {
    $self->deployOneComponent(
	$dep->{'groupId'}, $dep->{'artifactId'}, $dep->{'appversion'});
  }
}

sub deployOneComponent {
  my ($self, $groupId, $artifactId, $appversion) = @_;
  my $rootdir = $self->{'rbcconfig.repository.dir'};
    print "getting " . $self->{'rbcconfig.deployserver.url'} . "/maven/${groupId}/jars/${artifactId}-${appversion}.jar ...";
    my $jarFileContent = get $self->{'rbcconfig.deployserver.url'} . "/maven/${groupId}/jars/${artifactId}-${appversion}.jar";
    die $self->{'rbcconfig.deployserver.url'} . "/${groupId}/jars/${artifactId}-${appversion}.jar" . " does not exist or size 0, exiting.." if !defined($jarFileContent);
    print "done\n";
    TPRUN::Util->writeFile($self->getJarFile($groupId, $artifactId, $appversion), $jarFileContent);
    print "getting " . $self->{'rbcconfig.deployserver.url'} . "/maven/${groupId}/poms/${artifactId}-${appversion}.pom ...";
    my $pomFileContent = get $self->{'rbcconfig.deployserver.url'} . "/maven/${groupId}/poms/${artifactId}-${appversion}.pom";
    TPRUN::Util->writeFile($self->getPomFile($groupId, $artifactId, $appversion), $pomFileContent) if defined($pomFileContent);
    print "done\n";
}

sub isDeployed {
  my ($self) = @_;
  return undef if (!-f $self->getMainPomFile || !-f $self->getMainJarFile);
  foreach my $dep ($self->getDependencies()) {
    return undef if (! -f $self->getJarFile($dep->{'groupId'}, $dep->{'artifactId'}, $dep->{'appversion'}));
  }
  return 1;
}

sub getClassPath {
  my $self = shift;
  $ENV{'CLASSPATH'} = '' if (!defined($ENV{'CLASSPATH'}));
  my $classpath = $ENV{'CLASSPATH'}
	. TPRUN::Util->getClassPathSeparator()
	. TPRUN::Util->getCanonicalFilename($self->getMainJarFile());
  my $ls = TPRUN::Util->getCommand("ls");
  foreach my $jar (`$ls $self->{'instance.home'}/jars/*.jar`) {
    chomp($jar);
    $classpath .= TPRUN::Util->getClassPathSeparator() . $jar;
  }

  foreach my $dep ($self->getDependencies()) {
    $classpath .= 
	TPRUN::Util->getClassPathSeparator() 
		. TPRUN::Util->getCanonicalFilename(
			$self->getJarFile($dep->{'groupId'}, 
			$dep->{'artifactId'}, $dep->{'appversion'}));
  } 
  return $classpath;
}

sub getDependencies {
  my $self = shift;
  my $result;
  my $r = XMLin($self->getMainPomFile());
  foreach my $dep (@{$r->{dependencies}->{dependency}}) {
    $dep->{'groupId'} = TPRUN::Util->cleanUpString($dep->{'groupId'});
    $dep->{'artifactId'} = TPRUN::Util->cleanUpString($dep->{'artifactId'});
    $dep->{'appversion'} = TPRUN::Util->cleanUpString($dep->{'version'});
  }
  return @{$r->{dependencies}->{dependency}};
}

sub getMainPomFile {
  my $self = shift;
  return $self->getPomFile($self->{'rbcconfig.groupid'}, $self->{'rbcconfig.appname'}, $self->{'rbcconfig.appversion'});
}

sub getMainJarFile {
  my $self = shift;
  return $self->getJarFile($self->{'rbcconfig.groupid'}, $self->{'rbcconfig.appname'}, $self->{'rbcconfig.appversion'});
}

sub getPomFile {
  my ($self, $groupId, $artifactId, $appversion) = @_;
  return $self->getFile($groupId, $artifactId, $appversion, "pom");
}

sub getJarFile {
  my ($self, $groupId, $artifactId, $appversion) = @_;
  return $self->getFile($groupId, $artifactId, $appversion, "jar");
}

sub defaults {
  my $self = shift;
  $self->SUPER::defaults();
  
  # default JRE parameters
  #$self->setIfNull('rbcconfig.args', '-Xnoclassgc -server -Xms1024m -Xmx2048m');

  # if java debug is enabled
  #if(defined($ENV{'JAVA_DEBUG_PORT'})) {
  #  $self->{'rbcconfig.args'} .= " -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=$ENV{JAVA_DEBUG_PORT}";
  #}
}

sub run {
  my $self = shift;
  $self->SUPER::run();
  #$self->setEnvironmentVariables();  
  $ENV{'JAVA_HOME'} = $self->{'rbcconfig.javahome'};
  $ENV{'CLASSPATH'} = $self->getClassPath();

  my $cmd = "$self->{'rbcconfig.javahome'}/bin/java -DConfigFileName=$self->{'instance.home'}/config/env.xml -DAppProperties=$self->{'instance.home'}/config/instance.properties ";
  $cmd .= $self->{'rbcconfig.args'} if(defined($self->{'rbcconfig.args'}));
  $cmd .= " " . $self->{'rbcconfig.mainclass'};
  $cmd .= " " . $self->{'rbcconfig.args1'} if (defined($self->{'rbcconfig.args1'}));
  $self->log(`env`);
  $self->log("$cmd\n");
  exec($cmd);
}

1;

package TPRUN::Util;
use strict;
use POSIX;
use File::Basename;
use Time::HiRes qw(gettimeofday);
use Proc::ProcessTable;

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT = qw(cleanUpString getCanonicalFilename);

our $commands;

$commands->{'Linux'}->{'sendmail'} 	= '/usr/sbin/sendmail';
$commands->{'SunOs'}->{'sendmail'} 	= '/usr/lib/sendmail';
$commands->{'Linux'}->{'whoami'} 	= '/usr/bin/whoami';
$commands->{'SunOS'}->{'whoami'} 	= '/usr/ucb/whoami';
$commands->{'Linux'}->{'gunzip'} 	= '/usr/bin/gunzip';
$commands->{'Linux'}->{'tar'} 		= '/bin/tar';
$commands->{'Linux'}->{'rm'} 		= '/bin/rm';
$commands->{'Linux'}->{'mv'} 		= '/bin/mv';
$commands->{'Linux'}->{'mkdir'}		= '/bin/mkdir';
$commands->{'Linux'}->{'rlwrap'}	= '/home/tptools/Linux/x86_64/bin/rlwrap';

$commands->{'All'}->{'ls'}		= '/bin/ls';
$commands->{'All'}->{'find'} 		= '/usr/bin/find';
$commands->{'All'}->{'cp'} 		= '/bin/cp';

sub runOSCommand {
  my ($self, $cmd) = @_;
  print "Running $cmd ..\n" if(defined($debug));
  my @lines = `$cmd`;
  print @lines if(defined($debug));
  return @lines;
}

sub getCommand {
  my ($self, $name) = @_;
  my $sysname = TPRUN::Util->getSysName();
  return $commands->{$sysname}->{$name} if(defined($commands->{$sysname}->{$name}));
  return $commands->{'All'}->{$name} if(defined($commands->{'All'}->{$name}));
  print STDERR "Can't find $name for $sysname\n";
  print STDERR "Exiting...";
  exit(1);
  return undef; 
}

sub getSysName {
  my ($sysname, $nodename, $release, $version, $machine) = POSIX::uname();
  return $sysname;
}

sub getOSRelease {
  my ($sysname, $nodename, $release, $version, $machine) = POSIX::uname();
  return $release;
}

sub createDir {
  my ($self, $dir) = @_;
  my $mkdir = TPRUN::Util->getCommand("mkdir");
  my $cmd = "$mkdir -p $dir";
  print "$cmd\n" if(defined($debug));
  system $cmd;
}

sub overwriteDir {
  my ($self, $src, $dest) = @_;
  if(! -d $src || ! -d $dest) {
    print STDERR "Either $src or $dest does not exist! Not overwriting anyting!\n";
    return;
  }
  print "Overwritting trunk ...\n" if(defined($debug));
  my $tar = TPRUN::Util->getCommand("tar");
  my $cmd = "cd $src; $tar cf - . | (cd $dest; $tar xf -)";
  TPRUN::Util->runOSCommand($cmd);
}

sub cleanUpString {
  my ($self, $str) = @_;
  if($self ne 'TPRUN::Util') { $str = $self; }
  $str =~ s/^[\n|\r|\s]*//;
  $str =~ s/[\n|\r|\s]*$//;
  return $str;
}

sub getCanonicalFilename {
  my ($self, $filepath) = @_;
  # take out trailing /
  $filepath =~ s/\/$//;
  if($self ne 'TPRUN::Util') { $filepath = $self; }
  my $dir = dirname($filepath);
  $dir = `cd $dir; pwd`; chomp($dir);
  my $basename = basename($filepath);
  return "${dir}/${basename}";
}

sub writeFile {
  my ($self, $filename, $content) = @_;
  if(!defined($content)) { print "$filename empty!\n"; }
  my (@fields) = split('/',$filename);
  $#fields --;
  my $dir = join('/', @fields);
  #system "mkdir -p $dir";
  TPRUN::Util->createDir($dir);
  open(OUTFILE, "> $filename") || die $!;
  print OUTFILE $content;
  close OUTFILE;
}

sub getClassPathSeparator {
  return TPRUN::Util->getEnvironmentVariableFieldSeparator();
}

sub getEnvironmentVariableFieldSeparator {
  return ($^O eq 'cygwin') ? ';' : ':';
}

sub prependToEnvironmentVariable {
  my ($self, $name, $value) = @_;
  if(defined($ENV{$name})) {
    $ENV{$name} = $value . TPRUN::Util->getEnvironmentVariableFieldSeparator()
	. $ENV{$name};
  } else {
    $ENV{$name} = $value;
  }
}

sub appendToEnvironmentVariable {
  my ($self, $name, $value) = @_;
  if(defined($ENV{$name})) {
    $ENV{$name} = $ENV{$name} . TPRUN::Util->getEnvironmentVariableFieldSeparator() . $value;
  } else {
    $ENV{$name} = $value;
  }
}

sub readFirstLine {
  my ($self, $file) = @_;
  my $fh = new IO::File "< $file";
  warn "Can't open $file for read: $!" if (!defined($fh));
  return if (!defined($fh));
  my $line = $fh->getline; chomp($line);
  $fh->close();
  return $line;
}

sub showProcess {
  my ($self, $p, $label) = @_;
  printf("%-6s %-10s %-8s %-24s %s (%s)\n",
	$p->pid, $p->ttydev, $p->state,
	scalar(localtime($p->start)), $p->cmndline, $label)
		if(defined($p));
}

sub getProcessByPID {
  my ($self, $pid) = @_;
  if (defined($pid) && $pid =~ m/^\d+/) {
    my $pp = new Proc::ProcessTable;
    foreach my $p (@{$pp->table}) {
      return $p if($p->pid == $pid);
    }
  }
  return undef;
}

sub email {
  my ($self, $email, $subject, $message) = @_;
  return if(!defined($email) or !defined($subject) or $email eq '');
  my $sendmail = TPRUN::Util->getCommand('sendmail');
  return if(!defined($sendmail));
  open(SENDMAIL, "|$sendmail $email") || warn "Can't open sendmail! $!\n";
  print SENDMAIL "Subject: $subject\n";
  print SENDMAIL "To: $email\n";
  print SENDMAIL "$message";
  close SENDMAIL;
  #open(OUTFILE, ">>/tmp/email.out") or die "$!";
  #print OUTFILE $message;
  #close OUTFILE;
}

sub ask {
  my ($self, $question, @answers) = @_;
  print "=======> ${question}\n"; 
  for(my $i=0; $i<=$#answers; $i++) {
    print "  " . $i+1 . ") " . $answers[$i] . "\n";
  }
  my $answer = "";
  if($#answers == -1) {
    $answer = <STDIN>;
  } else {
    while($answer !~ m/^\d+/ or $answer > $#answers + 1) {
      $answer = <STDIN>;
    }
  }
  chomp($answer);
  return $#answers == -1 ? $answer : $answers[$answer-1];
}

sub getProcessStatus {
  my ($self, $pid, $cpid) = @_;
  return undef if(!defined($pid) or $pid !~ m/^\d+/);
  return undef if(!defined($cpid) or $cpid !~ m/^\d+/);
  my $sysname = $self->getSysName();   
  my $release = $self->getOSRelease();
  if($sysname eq 'Linux') {
    return $self->getRedhat3ProcessStatus($pid,$cpid) if($release =~ m/^2.4/);
    return $self->getRedhat4ProcessStatus($pid) if($release =~ m/^2.6/);
  } else {
    print STDERR "getProcessStatus is only supported on Linux\n";
    return;
  }
}

sub getTimeString {
  my $self = shift;
  my @timeData = localtime;
  my $hour = ${timeData[2]};
  my $minute = ${timeData[1]};
  my $second = ${timeData[0]};

  if($hour < 10) { $hour = "0${hour}"; }
  if($second < 10) { $second = "0${second}"; }
  if($minute < 10) { $minute = "0${minute}"; }

  return "${timeData[2]}:${timeData[1]}:${timeData[0]}";

}

sub getRedhat3ProcessStatus {
  my ($self, $pid, $cpid) = @_;
  my @lines = `ps -m -A -o pid,ppid,%cpu,rss,vsize,cmd`;
  my $result;
  $result->{threads} = 0;
  $result->{cpu} = 0;
  my $matched = 0;
  foreach my $line (@lines) {
    chomp($line);
    $line = " $line";
    my ($dum,undef,$ppid,$cpu,$rss,$vsize,@cmd) = split(/\s+/, $line);
    if($pid eq $ppid or $cpid eq $ppid) {
      $result->{threads} ++;
      $result->{cpu} += $cpu;
      $result->{rss} = defined(${rss}) ? ${rss} : -1;
      $result->{vsize} = defined(${vsize}) ? ${vsize} : -1;
      $matched = 1;
    }
  }
  $result->{rss} = defined($result->{rss}) ? $result->{rss} : -1;
  $result->{vsize} = defined($result->{vsize}) ? $result->{vsize} : -1;
  $result->{time} = $self->getTimeString();
  return undef if($matched == 0);
  return $result;
}

sub getRedhat4ProcessStatus {
  my ($self, $pid) = @_;
  my @lines = `/bin/ps -o pid,ppid,%cpu,rss,vsize,nlwp,cmd -p $pid`;
  #my $line = `/bin/ps -o pid,ppid,%cpu,rss,vsize,nlwp,cmd -p $pid | tail -1`;
  #$line = " $line";
  return undef if ($#lines == 0);
  my $line = " $lines[$#lines]";
  my (undef, undef, $ppid, $cpu, $rss, $vsize, $nThreads, @cmd) = split(/\s+/, $line);
  my $result;
  $result->{threads} = $nThreads;
  $result->{cpu} = $cpu;
  $result->{rss} = defined(${rss}) ? ${rss} : -1;
  $result->{vsize} = defined(${vsize}) ? ${vsize} : -1;
  $result->{time} = $self->getTimeString();
  return $result;
}

sub findTimeout {
	my $self=shift;

        my @local_time = localtime();

        my $sec_from_midnight = ($local_time[2] * 3600) + ($local_time[1] * 60) + $local_time[0];
	my $sec_at_midnight = 24 * 3600;
	my $sec_to_midnight = $sec_at_midnight - $sec_from_midnight;

	$sec_to_midnight = 24*3600 if (!$sec_to_midnight);
	return $sec_to_midnight;
}

1;

package TPRUN::SVN;

sub prepare {
  my ($self,$app) = @_;
  my $rm = TPRUN::Util->getCommand("rm");
  my $cmd = "${rm} -rf " . $app->svn_get_tmp_dir();
  TPRUN::Util->runOSCommand($cmd);
  TPRUN::Util->createDir($app->svn_get_tmp_dir());
  TPRUN::Util->createDir($app->svn_get_tmp_trunk_dir());
  TPRUN::Util->createDir($app->svn_get_tmp_tags_dir());
  TPRUN::Util->createDir($app->svn_get_tmp_branches_dir());
}

sub list_tags {
  my ($self, $app) = @_;
  my $cmd = "$app->{'rbcconfig.command.svn'} list " . $app->svn_get_tags_url();
  my @lines = `$cmd`;
  foreach my $tag (@lines) {
    chomp($tag); chop($tag);
    my $cmd1 = "$app->{'rbcconfig.command.svn'} log " . $app->svn_get_tags_url() . "/${tag}";
    my @lines1 = `$cmd1`; chomp(@lines1);
    my @fields = split(/ \| /, $lines1[1]);
    print "$tag | $fields[0] | $fields[1] | $fields[2] | $lines1[3]\n";
  }
}

sub getReleaseByTag {
  my ($self, $app, $tag) = @_;
  my $cmd = "$app->{'rbcconfig.command.svn'} log " . $app->svn_get_tags_url(). "/${tag}";
  my @lines = `$cmd`; chomp(@lines);
  my @fields = split(/ \| /, $lines[1]);
  $fields[0] =~ s/^r//; chomp($fields[0]);
  return $fields[0];
}

sub getNextTag {
  my ($self, $app, $appversion) = @_;
  my $cmd = "$app->{'rbcconfig.command.svn'} list " . $app->svn_get_tags_url();
  print "$cmd\n" if(defined($debug));
  my @lines = `$cmd 2> /dev/null`;
  my $latest_release = 0;
  foreach my $line (@lines) {
    chomp($line); chop($line);
    my @fields = split(/-/, $line);
    my $release = pop(@fields);
    my $bversion = join('-', @fields);
    if($bversion eq $appversion && $release > $latest_release) { $latest_release = $release; } 
  }
  return $latest_release + 1;
}

sub createRepository {
  my ($self,$config) = @_;
  my $ua = LWP::UserAgent->new;
  my $response = $ua->get("$config->{'rbcconfig.svn_create_url'}?instance=$config->{'instance.name'}");
  my $result;
  if($response->is_success) {
    foreach my $line (split(/\n/,$response->content)) {
      my @fields = split(/=/,$line);
      next if($#fields < 1);
      my $key = $fields[0];
      splice(@fields, 0, 1);
      $result->{$key} = join("=", @fields);
    }
  } else {
    $result->{'CODE'} = 'FAILED';
    $result->{'COMMENTS'} = $response->status_line;
  }
  return $result;
}

sub overwriteTrunk {
  my ($self,$app) = @_;
  TPRUN::Util->overwriteDir("$app->{'instance.home'}/config", $app->svn_get_tmp_trunk_dir());
}

sub overwriteBranch {
  my ($self, $app, $appversion) = @_;
  TPRUN::Util->overwriteDir("$app->{'instance.home'}/config", $app->svn_get_tmp_branch_dir($appversion));
}

sub prepareNewFiles {
  my ($self, $app, $dir) = @_;
  foreach (`cd $dir && $app->{'rbcconfig.command.svn'} status`) {
    chomp;
    my ($status, $file) = split(/\s+/);
    TPRUN::Util->runOSCommand("cd $dir && $app->{'rbcconfig.command.svn'} add $file") if ($status eq '?');
    TPRUN::Util->runOSCommand("cd $dir && $app->{'rbcconfig.command.svn'} delete $file") if ($status eq '!');
  }
}

# delete files other than svn files
sub cleanUpFiles {
  my ($self, $app, $dir) = @_;
  foreach (`cd $dir && find . -type f -print`) {
    chomp;
    if(! m/\.svn/) {
      unlink "$dir/$_"; 
    }
  }
}

sub overwriteConfigFromTag {
  my ($self, $app, $version) = @_;
  TPRUN::Util->overwriteDir($app->svn_get_tmp_tag_dir($version), "$app->{'instance.home'}/config");
  $app->cleanUpSVNDirFromConfig();
}

sub importTrunk {
  my ($self, $app) = @_;
  print "Importing trunk ...\n";
  my $cmd = "cd $app->{'instance.home'}; " . $app->{'rbcconfig.command.svn'} . " import " . $app->svn_get_tmp_dir_name() . " " . $app->svn_get_repo_url . " -m \"initial import\"";
  TPRUN::Util->runOSCommand($cmd);
}

sub commitTrunk {
  my ($self, $app, $comments) = @_;
  print "Commiting branch ..\n";
  my $cmd = "cd " . $app->svn_get_tmp_trunk_dir() . "; $app->{'rbcconfig.command.svn'} commit -m \"$comments\"";
  TPRUN::Util->runOSCommand($cmd);
}

sub diffBranch {
  my ($self, $app, $release) = @_;
  my $cmd = "cd " . $app->svn_get_tmp_branch_dir($app->{'rbcconfig.appversion'}) . "; $app->{'rbcconfig.command.svn'} -r $release diff ";
  return TPRUN::Util->runOSCommand($cmd);
}

sub commitBranch {
  my ($self, $app, $appversion, $comments) = @_;
  print "Commiting branch $appversion ...\n";
  my $cmd = "cd " . $app->svn_get_tmp_branch_dir($appversion) . "; $app->{'rbcconfig.command.svn'} commit -m \"$comments\"";
  TPRUN::Util->runOSCommand($cmd);
}

sub checkoutTrunk {
  my ($self, $app) = @_;
  print "Checking out ", $app->svn_get_trunk_url(), " under ", $app->svn_get_tmp_dir(), " ...\n" if(defined($debug));
  my $cmd = "cd " . $app->svn_get_tmp_dir() . "; $app->{'rbcconfig.command.svn'} checkout " . $app->svn_get_trunk_url();
  TPRUN::Util->runOSCommand($cmd);
}

sub checkoutBranch {
  my ($self, $app, $appversion) = @_;
  print "Checking out ", $app->svn_get_branches_url(), " under ", $app->svn_get_tmp_branches_dir(), " ...\n" if(defined($debug));
  my $cmd = "cd " . $app->svn_get_tmp_branches_dir() . "; $app->{'rbcconfig.command.svn'} checkout " . $app->svn_get_branches_url() . "/${appversion}";
  TPRUN::Util->runOSCommand($cmd);
}

sub checkoutTag {
  my ($self, $app, $version) = @_;
  print "Checking out ", $app->svn_get_tags_url(), " under ", $app->svn_get_tmp_tags_dir(), " ...\n" if(defined($debug));
  my $cmd = "cd " . $app->svn_get_tmp_tags_dir() . "; $app->{'rbcconfig.command.svn'} checkout " . $app->svn_get_tags_url() . "/${version}";
  TPRUN::Util->runOSCommand($cmd);
}

sub createBranchFromTrunk {
  my ($self, $app, $branchName,$comments) = @_;
  print "SVN copying from trunk to branch $branchName ...\n";
  my $cmd = "$app->{'rbcconfig.command.svn'} copy " . $app->svn_get_repo_url() . "/trunk " . $app->svn_get_repo_url() . "/branches/$branchName -m \"$comments\"";
  TPRUN::Util->runOSCommand($cmd);
}

sub createTag {
  my ($self, $app, $branch, $tag, $comments) = @_;
  print "SVN copying from branch $branch to tag $tag ...\n";
  my $cmd = "$app->{'rbcconfig.command.svn'} copy " . $app->svn_get_repo_url() . "/branches/$branch " . $app->svn_get_repo_url() . "/tags/$tag -m \"$comments\"";
  TPRUN::Util->runOSCommand($cmd);
}

sub ifExists {
  my ($self, $app, $url) = @_;
  my $cmd = "$app->{'rbcconfig.command.svn'} list $url";
  my @lines = `$cmd 2>/dev/null`;
  if($? == 0) { return "YES"; }
  return "NO";
}

1;

use strict;
use Getopt::Long;
use POSIX;
use FileHandle;
use Date::Manip;
use File::Basename;
use Digest::SHA1  qw(sha1 sha1_hex sha1_base64);

$|=1;

if($#ARGV == -1) { &help; exit(1); }

our $DIRNAME = "/usr/bin/dirname";
our $BASENAME = "/bin/basename";
our $VERSION = "1.0.12";

our ($config,$tibrvhome,$comments,$user);
our $sleepForQuit = 2;		# for Java to thread dump
my ($killall,$restartall,$restartapp,$disableretry,$enableretry,$status,$help,$vilog,$lesslog,$taillog,$viclog,$lessclog,$tailclog,$admin,$adminCmd,$threadDump,$adminurl,$adminInst,$version,$classpath,$coredump,$get,$create,$createfile,$performance,$svn,$checkin,$checkout,$forcestop,$import,$diff,$list,$clone,$after,$appversion,$appenv,$at,$instance,$heap,$vcs,$console,$setup_logger);
our ($daemon,$dummyv);

&checkUsage;

my $curdir = `$DIRNAME $0`;  chomp($curdir);
my $adir = `cd $curdir; pwd`; chomp($adir); $curdir = $adir;
my $rootdir = `cd $curdir/..; pwd`; chomp($rootdir);
my $confFile = "$rootdir/config/instance.properties";

my $provided_option = GetOptions(
	"killall"	=> \$killall,
	"stop"		=> \$killall,
	"create"	=> \$create,
	"createfile=s"	=> \$createfile,
	"restartall"	=> \$restartall,
	"restartapp"	=> \$restartapp,
	"disableretry"	=> \$disableretry,
	"daemon"	=> \$daemon,
	"start"		=> \$daemon,
	"debug"		=> \$debug,
	"admin"		=> \$admin,
	"adminurl=s"	=> \$adminurl,
        "admincmd=s"	=> \$adminCmd,
        "admininst=s"	=> \$adminInst,
	"app"		=> \$dummyv,
	"get"		=> \$get,
	"heap"		=> \$heap,
	"help"		=> \$help,
	"forcestop"	=> \$forcestop,
	"usage"		=> \$help,
	"vilog"		=> \$vilog,
	"lesslog"	=> \$lesslog,
	"taillog"	=> \$taillog,
	"viclog"	=> \$viclog,
	"lessclog"	=> \$lessclog,
	"tailclog"	=> \$tailclog,
	"enableretry"	=> \$enableretry,
	"threaddump"	=> \$threadDump,
	"coredump"	=> \$coredump,
	"performance"	=> \$performance,
	"version"	=> \$version,
	"classpath"	=> \$classpath,
	"status"	=> \$status,
	"svn=s"		=> \$svn,
	"comments=s"	=> \$comments,
	"user=s"	=> \$user,
	"diff"		=> \$diff,
	"import"	=> \$import,
	"checkin"	=> \$checkin,
	"checkout=s"	=> \$checkout,
	"list"		=> \$list,
	"clone=s"	=> \$clone,
	"after"		=> \$after,
	"vcs"		=> \$vcs,
	"appversion"	=> \$appversion,
	"appenv"        => \$appenv,
	"console"	=> \$console,
	"setuplogger"	=> \$setup_logger,
);

if (!$provided_option) {
	&help; exit(0);
}

if(defined($create)||defined $createfile) { TPRUN::Instance->create($createfile); exit(0); }
if(defined($clone)) { TPRUN::Instance->clone($clone); exit(0); }
if(defined($version)) { print "$VERSION\n"; exit(0); }
if(defined($debug)) { $debug = 1; }
if(defined($help)) { &help; exit(0); }
if(defined($adminCmd)) { $config->{adminCmd} = $adminCmd; }
if(defined($adminurl)) { $config->{adminurl} = $adminurl; }
if(defined($adminInst)) { $config->{adminInst} = $adminInst; }

my $APP = TPRUN::Instance->new($confFile);

if ($#ARGV ne -1 && !( ($#ARGV eq 0) && ($ARGV[0] eq $APP->{'instance.name'})) ) {
	&help; exit(0);
}

$APP->{'runtime'}->{'daemon'} = $daemon;
$APP->{'runtime'}->{'console'} = 1 if (defined $console);

$config = $APP;
if(defined($appversion)) { $APP->showAppVersion(); exit(0);}
if(defined($appenv)) { $APP->showEnvironment(); exit(0); }
if(defined($performance)) { $APP->showPerformance(); exit(0);}
if(defined($svn)) { $APP->svn($svn); exit(0); }
if(defined($list)) { $APP->svn('list'); exit(0); }
if(defined($checkin)) { $APP->checkin($user,$comments); exit(0); }
if(defined($checkout)) { $APP->checkout($checkout); exit(0); }
if(defined($get)) { $APP->deploy(); exit(0); }
if(defined($adminCmd)) { $config->{adminCmd} = $adminCmd; }
$config->{rootdir} = $rootdir;
if(defined($admin) && !defined($adminurl)) { $APP->admin(); exit(0);} 
if(defined($adminurl) && !defined($adminCmd)) { $APP->adminByURL($adminurl); exit(0);}
if(defined($adminCmd) && !defined($adminurl)) { $APP->adminCmd($adminCmd); exit(0) }
if(defined($adminCmd) && defined($adminurl)) { $APP->adminCmdByURL($adminurl,$adminCmd); exit(0) }
if(defined($admin) || defined($adminCmd) || defined($adminurl)) { &admin($config); exit(0); }
if(defined($classpath)) { print $APP->getClassPath(), "\n"; exit(0); }
if(defined($status)) { exit $APP->status($vcs); }
if(defined($killall)) { $APP->killall(); exit(0); }
if(defined($forcestop)) { $APP->killall($forcestop); exit(0); }
if(defined($threadDump)) { $APP->threadDump(); exit(0); }
if(defined($heap)) { $APP->dumpHeap(); exit(0); }
if(defined($coredump)) { $APP->coredump(); exit(0); }
if(defined($restartall)) { $APP->restartall(); exit(0); }
if(defined($restartapp)) { $APP->restartapp(); exit(0); }
if(defined($disableretry)) { $APP->disableretry(); exit(0); }
if(defined($enableretry)) { $APP->enableretry(); exit(0); }
my $logdir = $APP->getLogDir();
if(defined($vilog)) { exec "view $logdir/Daily"; }
if(defined($lesslog)) { exec "less $logdir/Daily"; }
if(defined($taillog)) { exec "tail -f $logdir/Daily"; }
if(defined($viclog)) { exec "view $config->{'piddir'}/c.log"; }
if(defined($lessclog)) { exec "less $config->{'piddir'}/c.log"; }
if(defined($tailclog)) { exec "tail -f $config->{'piddir'}/c.log"; }
if(defined($diff)) { $APP->svn('diff'); exit(0); }
if(defined($import)) { $APP->svn('import'); exit(0); }
if(defined($daemon) && (!defined($APP->{'rbcconfig.batch'}) or $APP->{'rbcconfig.batch'} ne 'yes') && (! defined $console) ) { if(fork()) { exit(0); } }
if(defined $setup_logger) {
	$APP->setupLogger(); 
	$APP->cleanUpLogs();
	exit(0); 
}

$APP->checkPids();
$APP->respawn() if !defined $console;
#$APP->checkFingerPrints();
$APP->setup();
&setup();
&iterate($config);

exit 0;

sub help {
  print "Usage: $0 -<option>\n";
  print "The following options are supported:\n";
  print "\t-admin			connect to admin shell\n";
  print "\t-admincmd \"cmd\"		run cmd in admin shell and return results\n";
  print "\t-adminurl \"url\"		connect to remote admin shell by host:port, it works with -admincmd option\n";
  print "\t-admininst \"inst\"	connect to remote admin shell by instance name, it works with -admincmd option\n";
  print "\t-appversion 		show app and config version\n";
  print "\t-appenv             	 show app environment\n";
  print "\t-checkin			check in configuration\n";
  print "\t-checkout			check out configuration\n";
  print "\t-create			create a new instance\n";
  print "\t-debug			turn on debug\n";
  print "\t-daemon/start		detach from the current tty\n";
  print "\t-disableretry		disables retry\n";
  print "\t-help/usage		print help message\n";	
  print "\t-enableretry		enables retry\n";
  print "\t-killall/stop		stops both controller and the application\n";
  print "\t-lessclog		less controller log\n";
  print "\t-lesslog		less current day's application log\n";
  print "\t-restartall		restart application and reset retry count\n";
  print "\t-restartapp		restart application without resetting retry count\n";
  print "\t-status			shows status of controller and application\n";
  print "\t-tailclog		tail -f controller log\n";
  print "\t-taillog		tail -f current day's application log\n";
  print "\t-threaddump		cause java app to do a thread dump\n";
  print "\t-viclog			vi controller log\n";
  print "\t-vilog			vi current day's application log\n";
}

sub iterate {
  my $config = shift;
  my $started = "no";
  $config->{'rbcconfig.retrytimes'} = 0 if defined $config->{'runtime'}->{'console'};
  while($config->{'rbcconfig.retrytimes'} >= 0 && 
	$config->{'rbcconfig.spawn'} == 1) {
    if($started eq "yes" && defined($config->{'rbcconfig.sleepBetweenRetries'}) && $config->{'rbcconfig.sleepBetweenRetries'} =~ m/^\d+/) {
      $APP->log("sleep for $config->{'rbcconfig.sleepBetweenRetries'} seconds before retry\n");
      sleep($config->{'rbcconfig.sleepBetweenRetries'});
    }
    &execute($config);
    $started = "yes";
    $config->{'rbcconfig.retrytimes'}--;
    $APP->log("iterate: after execute retrytimes remains $config->{'rbcconfig.retrytimes'}\n") if (!defined $config->{'runtime'}->{'console'});
  }
}

sub execute {
  my $config = shift;
  $config->{'cpid'} = $$;
  if($config->{'apid'} = fork()) {
    $APP->log("Parent wait pid on $config->{'apid'}..\n");
    $APP->writeApplicationPidFile($config->{'apid'});
	my %pids = ();
	$pids{$config->{'apid'}}=1;
	while (%pids) {
    		my $pid = waitpid($config->{'apid'}, 0);
		delete $pids{$pid} if defined $pids{$pid};
	}
    if(defined($config->{'rbcconfig.batch'}) && $config->{'rbcconfig.batch'} eq 'yes') {
      exit $?;
    }
    if(defined($?) && (!defined $config->{'runtime'}->{'console'})) {
      TPRUN::Util->email($config->{'rbcconfig.email'}, "$config->{'instance.name'} died unexpectedly!", "$config->{'instance.name'} died unexpectedly!");
    }
    $APP->log("$config->{'apid'} reaped $?\n");
  } else {
    $APP->run();
  }
}

sub setup {
  $SIG{QUIT}=$SIG{TERM}=$SIG{HUP} = \&catchHUP;
  $SIG{USR1}= \&catchUSR1;
  $SIG{USR2}= \&catchUSR2;
  $SIG{TSTP}= \&catchTSTP;
  $SIG{CONT}= \&catchCONT;
	$SIG{ALRM} = \&catchALARM;
}

# make sure the user did not find using the PATH
sub checkUsage {
  my $base = `$BASENAME $0`; chomp($base);
  if($base eq $0) {
    print("You have to type the path to tprun in order to start the application\n");
    exit 1;
  }
}

sub catchHUP {
  $APP->log("catchHUP($$): sending HUP to child process\n");
  $config->{'rbcconfig.retrytimes'} = 0;
  $config->{'rbcconfig.spawn'} = 0;
  if(defined($config->{'apid'})) { 
    if($config->{'rbcconfig.binarytype'} eq 'java') {
      kill SIGQUIT, $config->{'apid'}; sleep($sleepForQuit);
    }
    #kill SIGHUP, $config->{'apid'}; 
    $APP->killme();
    $APP->log("catchHUP($$): waiting on child $config->{'apid'} \n");
    waitpid($config->{'apid'}, 0);
    $APP->log("catchHUP($$): child $config->{'apid'} reaped\n");
  }
  $APP->cleanUpPidFiles();
  exit(0);
}

sub catchUSR1 {
  $APP->log("catchUSR1($$): sending HUP to child: $config->{apid}\n");
  kill SIGHUP, $config->{apid};
  waitpid($config->{apid},0);
  $APP->log("catchUSR1($$): reset retrytimes to $config->{originalRetrytimes}+1\n");
  $config->{'rbcconfig.retrytimes'} = $config->{originalRetrytimes}+1;
  $config->{'rbcconfig.spawn'} = 1;
}

sub catchUSR2 {
  $APP->log("catchUSR2($$): sending HUP to child: $config->{apid}\n");
  kill SIGHUP, $config->{apid};
  waitpid($config->{apid},0);
  $config->{'rbcconfig.retrytimes'}++; 
  $APP->log("catchUSR2($$): reset retrytimes to $config->{'rbcconfig.retrytimes'}\n");
}

sub catchTSTP {
  $APP->log("catchTSTP($$): disabling retry...\n");
  $config->{'rbcconfig.spawn'} = 0;
}

sub catchCONT {
  $APP->log("catchCONT($$): enabling retry...\n");
  $config->{'rbcconfig.spawn'} = 1;
}

sub catchALARM {
	$APP->log("ALARM HIT!\n");
	$APP->setupLogger();
	$APP->log("ALARM SERVICED!\n");
}

