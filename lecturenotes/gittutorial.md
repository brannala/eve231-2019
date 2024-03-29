# EVE 231 Lecture 2 (Tuesday Oct 1, 2019)

## Using Git to Maintain Your Files
The git program allows you to retain copies of all previous and current versions of your files without any clutter in your project directory. You can easily go back to earlier versions of your project if you accidentally deleted a file that you now need, or if you just want to check an earlier version of a file. Git allows you to keep a minimalist folder containing only the current files, but also have the security of knowing that all previous versions are still available should you need them. Git does this in an efficient way -- only recording a record of the differences from the previous versions at each commit (much like having a linked list of output from the UNIX ```diff``` command). It is also possible to maintain a copy of your repository on a remote machine for backup purposes.

### Creating a Git Repository
To start using git first create a directory for your project files
```
mkdir myproject
cd myproject
```
then create a new git repository in this directory
```
git init
```
This has created a git revision control system. All the files used by git are in a hidden subdirectory called .git and can be viewed using 
```
tree .git
```
The ```.git``` directory is a *hidden directory*. In UNIX any file or directory that begins with a . is not shown by the ```ls``` command. To see the hidden directories use ```ls -a```. If you want to change the directory back to a non-git directory simply delete the subdirectory .git using a command like ```rm -Rf .git```. Here the ```-R``` option tells ```rm``` to recursively remove the subdirectory and all the files within it (don't delete the directory yet, we still need it for the exercise). By the way, NEVER use the command ```rm -Rf /``` as root user or you will be a sad puppy. 

### Adding Content to the Repository
We will use the vintage ```ed``` editor to add a file to the directory
```
ed
a
this is a test
.
w test.txt
q
```
Now try the command
```
git status
```
This command tells you the status of the git repository. By default the files you place in the directory are not tracked by the git and must be added to the index of the repository, otherwise they are ignored by git -- this process of adding files to the repository list is known as *staging* the files. The command
```
git add test.txt
```
adds the file you created to the repository. If you have many new files you can add them all use the single command
```
git add --all
```
Now try ```git status``` again and you will see that your file has been *staged*. 

### Making a Commit
To save the file as a permanent part of the git tracking history you must now commit it. Each commit becomes a node in the permanent history of the repository. To commit the ```test.txt``` file use the command
```
git commit test.txt -m "first commit"
```
where the comment in parentheses is a human-readable explanation of the repository changes that becomes part of the commit records. If you have multiple tracked files
that have changed you can commit them all in one go using
```
git commit -a -m "many files have changed"
```
The advantage of committing individual files is that you can have individual comments associated with each commit. Also you can retrieve the previous file versions in 
a more fine-grain manner. We can now safely delete the file ```test.txt``` from the repository knowing that we have a permanent copy in the git chain. To delete the file use the command
```
rm test.txt
```
Now commit the change to the repository
```
git commit -a -m "removed test.txt"
```
To see a history of your commits use the command
```
git log
```
For my repository this produces
```
commit 47fc43da87e9651a091cbd9e93df1aa2bfd25e9d (HEAD -> master)
Author: Bruce <brannala@ucdavis.edu>
Date:   Sun Sep 22 11:43:17 2019 -0700

    deleted file

commit 5540e2b5a55d63240c9ee23b65e9aa600c5df843
Author: Bruce <brannala@ucdavis.edu>
Date:   Sun Sep 22 11:37:33 2019 -0700

    first commit
```
The commits are stored as a linked list; each commit has a child that has a link to its parent. The last commit is the current state (HEAD) of the repository
and also has the branch name (in this case ```master```) attached to it. There is also a unique 40 digit *hash* number associated with each commit that identifies it.

### Time Travelling with Git Checkout
Now that you have a git repository you can return to the state that it was in at any previous commit. The commit is identified by any unique subsequence of it's hash number.
To return *my* repository to the state it was in immediately before the last commit (when I deleted the file) I can use the command
```
git checkout 5540e
```
Voila! the test.txt file is now present again in the subdirectory. The command will be similar for your repository except that the hash number will be different (unique to your commits). To return to the current (most recent) commit simply type
```
git checkout master
```
If you are currently editing the files in your git repository and have not yet committed your changes but you want to revert the repository to a prior commit state you can first *stash* the edited files
```
git stash
```
This takes you back to the state at the most recent commit (master). You can then use checkout to switch to another commit. When you are done and have switched back to master you can retrieve your stashed file changes using
```
git stash pop
```
Lets give stash a try. First create a new file using ed
```
ed
a
test2
.
w test2.txt
q
```
and then add the file to the repository
```
git add test2.txt
```
and stash the changes
```
git stash
```
Using ```ls``` now shows no file ```test2.txt``` in the directory. Now pop it back
```
git stash pop
```
The file is back!

### Replicating Repositories with Git Clone
To create a now local copy of a git directory (complete with the commit history) use the ```git clone``` command. For example
```
cd ../
mkdir myprojectcopy
cd myprojectcopy
git clone ../myproject
```
This creates an identical copy of the repository. The *origin* of the repository is the original repository that was cloned. Often a repository is cloned to make temporary changes to files that will later be deleted. In that case, to get rid of the clone just use ```rm -Rf```. However, sometimes a clone is a *working copy* of a repository and commits made to that repository need to be added to the origin repository. If the origin repository has not been altered since the clone was created then the changes can be *pushed* to the origin repository without any conflicts -- this is called a *fast-forward*. Try creating a new file ```test3.txt``` in the cloned repository using the ```ed``` editor
```
cd myproject
ed
a
a third file
.
w test3.txt
q
```
and then add and commit the file
```
git add test3.txt
git commit -a
```
The files are now committed in the local repository. If you enter ```git status``` you will now see the following
```
On branch master
Your branch is ahead of 'origin/master' by 1 commit.
  (use "git push" to publish your local commits)

nothing to commit, working tree clean
```
The message now says that although there is nothing to commit the branch is ahead of ```origin/master```. Git knows that this is a clone and is warning you that it is now ahead of the origin.
We will try using the push command to propagate our commits to the remote repository as git recommends
```
git push
```
Oops. You will now see some error messages. The second to last line of the error message is
```
! [remote rejected] master -> master (branch is currently checked out)
```
Git is cowardly refusing to push changes because the remote branch ```origin master``` is currently checked out by a user (me) and if I have already made changes to the checked out branch the push could make a big mess.
The solution is to create a new repository branch on origin and switch to that branch before trying to push the changes from the clone. The solution will be given after the discussion of branches below.

### Git Branches
Think of a git repository as being like a tree. Trees can have one or more branches. A tree also has a root. The root of the git repository tree is the initial commit. Initially the repository has a single branch and the tip of the branch is called ```master```. When we make a commit a new node is added to the branch, the previous commit node become its parent node and the new node becomes ```master```. It is also possible to create a new branch; in that case the parent of the branch is the last commit made before the branch was created. Two branches now exist and we can make commits to either branch -- each branch has a separate history after the time of the split. It is possible to merge the commits on the branches later if desired but there may be conflicting changes that will need to then be resolved. To create a new branch called ```new_test``` and switch the current repository to the new branch use the command
```
cd ../../myproject
git branch new_test
git checkout new_test
```
The command
```
git branch
```
lists all the current branches. For my repository this produces
```
  master
* new_test
```
so there are now two branches ```master``` and ```new_test```. The current (active) branch is indicated by an asterisk (in this case ```new_test```). To switch back to the ```master```
branch use the command
```
git checkout master
```
A branch that is not currently checked out can be deleted using the --delete option of the branch command. We can delete the ```new_test``` branch using
```
git branch --delete new_test
```
Be careful, any commits on the branch that have not been merged will be lost when the branch is deleted.
It should now be clear how to solve our previous push problem -- create a new branch on ```origin``` and switch to it so that the ```master``` branch is no longer checked out
```
git branch working
git checkout working
```
then go back to the cloned project folder, switch to the ```master``` branch and make the push
```
cd ../myprojectcopy/myproject
git checkout master
git push
```
The push should now have been a success and changes you made on the clone have now been propagated to the ```origin``` repository.

### Merging Git Branches
Often it is useful to have at least two branches for each project: one that is the *stable* branch (often ```master```) which has the currently correct and useable versions of your files and another that is the *unstable* branch (for example ```working```) where you have the files that you are currently at work on. When the working files are completed and ready for the stable branch you merge them into that branch. If you are the only person working on the project and you always use the ```working``` branch for ongoing work and commits then there should be no changes or commits in the ```master``` directory since the last merge and therefore merging ```working``` into ```master``` is easy, it is simply replaying all the changes in ```working``` in the branch ```master```. As mentioned earlier this is called a *fast-forward* in git. We now try making some changes in our new branch of the ```myproject``` repository and merging them back into ```master```. Check out the ```working``` branch and use ed to create a new file
```
git checkout working
ed
a
a fourth file
.
w test4.txt
q
git add test4.txt
git commit -a -m "a fourth file was added"
```
Now try switching to the ```master``` branch and looking at the files
```
git checkout master
ls
```
There should be a file in the master branch called ```test3.txt``` but the file ```test4.txt``` is not present, it is currently only present in the ```working``` branch. We now try merging the ```working``` branch back into the ```master``` branch
```
git merge working
```
This will produce the output
```
Merge made by the 'recursive' strategy.
 test4.txt | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 test4.txt
```
Now try ```ls``` and you will see that ```test4.txt``` is part of the ```master``` branch. Also try ```git log```. You should see that the merge is listed in the log as well as the commit that was done in branch ```working```. One strategy is to never make direct changes to ```master``` and rely on merges to bring your changes into the ``master``` repository. If you try to merge a pair of branches that both include changes to the same files you will have to manually resolve any conflicts that occur in the modified files. We will not go into merge conflict resolution in this course. By following the strategy above you can avoid conflicts entirely. 

### Remote Repositories
This tutorial is not quite over. Thus far, everything we have done with our git repositories has been on our local machine. You now have some great skills for file management. You know how to create a repository for your files, add files to the repository, commit your file changes, create one or more working (unstable) branches, merge the unstable branches back into the stable branch, and recover older versions of your work from previous commits. But what if your hard drive fails? You will lose your local git repository and all your work! You do not yet have a backup. It is quite possible to just create an archive of your git directory every evening (using tar for example) and then copy the archive to a remote machine (using sftp for example). That works strategy works, but you will need an account on a machine that allows you to store your data and copying a large tarred git directory every day may be slow (even if the tar archive is compressed). A more efficient strategy would be to use a tool like rsync that is specifically designed to allow incremental backups by synchronizing files between machines. But wouldn't it be better to make incremental backups of just the bits of files that have changed and do that every time you commit your git files so you are sure to have a backup of your current commits always? Yes it would, and git can help you do that (and it can be done for free!) -- the solution is to create a remote repository. 

Recall that when we cloned ```myproject``` the ```origin``` variable of the clone was the parent repository of the clone. The same thing happens if you create a local repository by cloning a repository on a remote machine. The ```origin``` is now the remote machine. That is the simplest way to create a new repository with a remote origin to which changes can be pushed: simply create an empty repository on a remote machine and clone it on your local machine. How do you create a repository on a remote machine for cloning? One easy way is to create a free account on github or bitbucket. Students can apply for a free [Github Student Developer Pack](https://education.github.com/pack) that allows an unlimited number of public and private repositories. A free GitHub account normally only allows public repositories. There are also free accounts available on [bitbucket](https://bitbucket.org/product/) that allow unlimited private repositories for sharing with up to 5 team members. 

### Creating a Remote Repository
To create your git repository with a remote repository at origin first create an account at a site such as GitHub. Log into your account on a web browser and follow the instructions to create a new empty repository. When the repository has been created you will see a link with a web address that you will copy for cloning. Create a folder for your git repositories then ```cd``` to the new folder and type ```git init``` to initialize the folder as a repository. Then enter the command ```git clone <address>``` where ```<address>``` is the repository web address that you copied. This will create a local clone of the repository. Put some files in the repository then commit and push them. To push files to a repository you must have permissions so you will be prompted for a password for your online git account when you push. If it is a private repository you will also be prompted for a password when you clone or pull the repository. Public repositories can be cloned and changes pulled without a password. If you create a new branch for your project called ```working```, check out the branch and commit some changes to it, you need to use the following commands to create the branch on the remote and push the changes
```
git push --set-upstream origin working
```
The notes and exercises for EVE 231 are on a public repository on github. To clone the repository to your local machine use the command
```
git clone https://github.com/brannala/eve231-2019.git
```
I will periodically add files to the repository during the course so whenever you want to work with files in the directory you should first get the latest commits by using the command
```
git pull
```
If you have made changes to files in the directory use stash to store your changes before pulling. Alternatively, you could simply delete the previously cloned directory and clone a new copy.

### Exercises
Create a local git repository on your machine that has a remote ```origin```, copy some of your important files to the repository and commit and push the changes. Then create an unstable working branch of your repository, make some edits, and then commit and push the changes.
