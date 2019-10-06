# EVE 231 Lecture 4 (Tues Oct 8)

## Custom UNIX Commands via Alias and Bash Shell Scripts

### Creating a new UNIX command using alias
An alias is a custom command usually used to call a longer UNIX command. Here is an example of a bash alias
```
alias c='clear'
```
This will create a new command ```c``` that clears the screen. It is easier to type ```c``` that it is to type ```clear``` if you frequently need to clear your screen.
You can remove the alias using the command 
```
unalias c
```
If you want to use an alias just for your current session then enter it at the command line as shown above. It will disappear when you leave the terminal (shell) session. On the other hand if you want a permanent alias put it into your .bash_profile or .bashrc file and it will be available every time you open a bash shell. For example, I frequently connect to my fat node lab server using ssh so I have a permanent alias for this in my .bashrc file
```
alias gofat='ssh bruce@fatty'
```
I don't need to give the full name or ip address for this machine because I have linked the name fatty to the machine's ip address by adding a line to the ```/etc/hosts``` file on my computer (you need to use sudo to edit ```/etc/hosts```). 
Once defined unix will also auto-complete the names of aliases so I can call this command with gof (TAB) as well. You can see a list of all your currently defined aliases by typing ```alias``` with no arguments.

### Creating a shell script
If you need to repeatedly eexcute a set of UNIX commands that require several lines and or takes variable input you should write a bash shell script rather than create an alias. My recommendation is that you create a subdirectory named ~/bin that will contain all your shell scripts and add the directory to PATH in your .bashrc or .bash_profile file. We will start with a simple script that just prints 'Hello World!' to the screen when it is called. Type ```emacs ~/bin/hello``` to create the file and edit it in emacs. Enter the following contents into the file and save it.
```
#!/bin/bash

# my first shell script
echo Hello World!
```
The first line of the file tells bash what shell to use to run the script (the bash shell executable is located at /bin/bash on most UNIX installations). The so-called shebang symbol ```#!``` must precede the path to the shell executable file. Comments can be included elsewhere in the script using the ```#``` symbol at the beginning of the comment line. Before we can run this script we must give ourselves executable privileges 
```
chown u+x ~/bin/hello
```
The program can now be executed by typing ```hello```. If that produces an error message it may be because ```~/bin``` is not in your PATH variable. Try ```~/bin/hello``` instead or add ```~/bin``` to PATH. So far, we have done nothing that we could not easily have done using alias. For example,
```alias hello='echo Hello World!'``` would have accomplished the same thing. What is we want a more personalized hello? Modify the shell script to instead be
```
#!/bin/bash

# my second shell script
echo Hello $1 $2!
```
The ```$1``` specifies the value contained in the first command line argument and $2 is the second. Now try running the script again with some arguments
```
hello Bruce Rannala
```
This returns 
```
Hello Bruce Rannala!
```
This shell script takes command line arguments. But what if no argument is provided? You can add an if statement to handle that situation gracefully
```
#!/bin/bash

# my second shell script
if [ -z "$1" ]
then
	echo Hello mystery person!
else
	echo Hello $1 $2!
fi

```
You can also make a script interactive, awaiting a response from the user 
```
#!/bin/bash

# my third shell script
echo What is your name?
read name
if [ -z "$name" ]
then
    echo You have no name!
else
    echo Hello $name!
fi
```
Here is a practical script that adds, commits and pushes all the files in a directory to a remote git repository
```
#!/bin/bash

read -r -p 'Commit message: ' desc  # prompt user for commit message
git add .                           # track all files
git add -u                          # track deletes
git commit -m "$desc"               # commit with message
git push origin master              # push to origin
```
All the UNIX commands that work in the bash shell can be used in a bash shell script. Any time you notice that you are repeatedly typing the same combinations of commands you should consider defining an alias or writing a bash script to make your life easier.

### Exercise 1
Write a shell script that creates a compressed tar archive (a.k.a tarball) that contains all your important configuration scripts and subdirectories with software configurations (I suggest including at least .bashrc [or .bash_profile] and the subdirectory .emacs.d). This could be useful if you are working on another UNIX machine such as a server  in your lab. 



