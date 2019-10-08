# Connecting to a Remote Machine and Transferring Files in UNIX

## Logging into a shell on a remote machine
There is an account on my server for you to try logging into. We will use the secure shell (ssh) to connect to the machine. This is a terminal connection protocol with an encrypted tunnel that replaces an unsafe older plain text protocol called telnet. Type the following command
```
ssh eve131@ipaddress
```
You will be prompted for a password that I will write on the board during class along with the ipaddress. You will all be logged into the same account so create a subdirectory for your own use. Here ```yourname``` is your first name
```
mkdir yourname
```
If you type ```ls``` you should see your subdirectory, a subdirectory called evetest, and possibly other subdirectories created by other students (recognize any names?). All the UNIX commands work in the bash shell on this remote machine as they would on your local machine. Now log out of your session by using the command 
```
logout
```

## Transferring files to and from and remote machine
We will now try transferring some files from the remote machine to your local machine. There is a file called eve313.tar that you will copy to your local machine -- we will do that using a few different programs. We will first use the Secure Copy Protocol which uses an encrypted connection like ssh. Type the command
```
scp eve131@ipaddress:~/evetest.tar ~/
```
This copies the ```evetest.tar``` file from the home directory of user eve131 into the home directory on your local machine. Test dearchive the file to ensure that it was transferred without corruption
```
tar -tvf ~/evetest.tar
```
We will now try an interactive transfer using the Secure File Transfer Protocol which is an encrypted version of the older plain text FTP program. Firs log into the remote machine
```
sftp eve131@ipaddress
```
Then transfer the file with the get command and then quit
```
get evetest.tar
quit
```
The file should now be in your current working directory. We will now upload the file evetest.tar into the local subdirectory ```~/yourname``` on the remote machine. First we will use scp
```
scp evetest.tar eve131@ipaddress:~/yourname/
```
The scp program can also copy files and directories recursively to (or from) a remote machine. To illustrate this first dearchive the evetest.tar file into your home directory
```
tar -xvf evetest.tar
```
This creates a hierarchical set of directories and files. To see them type ```tree ~/evetest```. We will recursively copy the files and directories to subdirectory ```~/yourname/``` on the remote machine
```
scp -r ~/evetest/ eve131@ipaddress:~/yourname/
```
Now log back into the remote machine and you should find the subdirectories and files in your personal folder.
