#Reversible Interpreter Guide
	
A Reversible Interpreter written in Haskell.

# Installation
Download and install Stack by executing the command `curl -sSL https://get.haskellstack.org/ | sh`

After installing Stack do
<pre><code>git clone https://github.com/CalvinNolan/ReversibleInterpreter.git
cd ReversibleInterpreter
stack ghci
</code></pre>

After the project is compiled enter
<pre><code>main
</code></pre>
This will begin interpreting the program in `program.txt`. Read the next section to learn how to interact with the program being interpreted.

#How to use the Interpreter

The Reversible Interpreter provides the functionality to run a program step by
step while being able to show the historical contents of all variables and stepping
backwards through the program at any point.

A sample program is already provided in the program.txt file, if you wish to
run your own through the interpreter, either replace the contents of this file
with a valid program or add a new file and alter the main function in the project
with the path to this new file containing the program to run.

After a line is executed, the interpreter will wait for an input for it's
next command. The possible inputs are:
  <br>
  `.` - This will execute the next line.
  <br>
  `,` - This will show the historical contents of all variables in the current program.
  <br>
  `/` - This will step backwards to the previous line and restore the state of the program to what it once was.

#Solutions
<b>1. Full stack project with a readme explaining it all!</b>

<b>2. Mondanic Interpreter Stepping</b>

To achieve this, two new functions, handleExec and userPrompt, were added. 
Every Seq Statement puts it's two child Statements through the handleExec function 
where the next statement is described and then the userPrompt function is called on 
that statement. The userPrompt function will wait for the user to input the '.' 
character to execute the next statement of the program.

<b>3. Inspect command</b>

Some extra functionality was added to the userPrompt function to now allow
the ',' character to output the values all recorded variables. To achieve this
the state being carried through the State Monad which represents the variables
is output in it's entirety.

<b>4. Variable History Inspection</b>

The structure of the state carried throughout the program was altered to be able
to carry more than one value for a variable at a time. This meant it was possible to
alter the state to hold a list of variable values for a given key. Using this new
feature, the state was changed to hold a list of historical values for each variable key. 
When a variable was altered, the new value was appended to the front of it's list. 
Now upon inspection, the entire list of a variables historical values are output in 
an ordered array, the first element indicating it's current value and the last 
elementing indicating it's oldest value.

<b>5. Step Backwards</b>

This functionality required carrying around a list of previously executed statements
throughout the program to allow for backtracking at any time. By combining this list 
of historical statements and the historical values kept in state it was possible to 
easily implement the reversible feature of the interpreter. The userPrompt function
was extended to allow the '/' character to signify a step backwards. To step backwards
the program abandons it's current execution and instead evaluates the previous statements
again through the reverseStatements function. If the previous statement was an Assign, 
it will remove the most recent value for that variable, returning it to it's state 
before the Assign statement. This is implemented in the new reverseSet function.
	
This method works perfectly for code with no flow control but runs into issues with
While loops where the executed statements of each loop need to be shared with the 
next loop. This causes problems as they're represented by different Sequences and
cannot share each others resulting executed statements. A better solution to this
problem would be to store the executed statements list in state where it is shared
across all scopes.
