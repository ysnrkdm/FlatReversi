import subprocess
import re

def go(proc):
    proc.stdin.write('go\n')
    output = proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    print output.rstrip()
    # X plays XX
    a = re.findall(r"(.+) plays ([a-zA-Z][0-9]|PS)", output.rstrip())
    print a
    return a[0][1]

def init(proc):
    proc.stdin.write('init\n')
    output = proc.stdout.readline()
    print output.rstrip()

def play(proc, hand):
    proc.stdin.write(hand + '\n')
    output = proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    # print output.rstrip()
    # X play XX
    a = re.findall(r"(.+) play ([a-zA-Z][0-9]|PS)", output.rstrip())
    # print a
    return a[0][1]

def quit(proc):
    proc.stdin.write('quit\n')
    output = proc.stdout.readline()
    print output.rstrip()
    remainder = proc.communicate()[0]
    print remainder
    # The proc should not be used after calling quit

# Returns true if it's game over, if not game over, returns false
def show(proc):
    proc.stdin.write('verbose 1\n')
    output = proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    output += proc.stdout.readline()
    print output.rstrip()
    b = re.findall(r".*(Black|White|black|white) won.*", output.rstrip())
    proc.stdin.write('verbose 0\n')
    output = proc.stdout.readline()
    a = re.findall(r".*(Game Over).*", output.rstrip())
    # print "game over?" + str(len(a))
    print output
    won = "None"
    if len(b):
        if len(b[0]):
            won = b[0]
    return (len(a) > 0, won)

def play_a_game():
    init(proc_a)
    init(proc_b)

    isGameOver = False

    while(not isGameOver):
        (isGameOver, won) = show(proc_a)
        if isGameOver:
            break
        (isGameOver, won) = show(proc_b)
        if isGameOver:
            break
        ha = go(proc_a).lower()
        # print 'proc_a plays [' + ha + ']'
        play(proc_b, ha)
        (isGameOver, won) = show(proc_a)
        if isGameOver:
            break
        (isGameOver, won) = show(proc_b)
        if isGameOver:
            break
        hb = go(proc_b).lower()
        # print 'proc_b plays [' + hb + ']'
        play(proc_a, hb)
    quit(proc_a)
    quit(proc_b)

    return won

black_won = 0
white_won = 0

for i in range(0,1):
    print 'Starting engines...'
    proc_a = subprocess.Popen('/Users/yoshinori/Documents/OneDrive/projects/othello/edax/4.3.2/bin/mEdax -q -l 1', shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    proc_b = subprocess.Popen('/Users/yoshinori/Documents/OneDrive/codes/FlatReversi/Hamlet/dist/build/Hamlet/Hamlet', shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    # proc_b = subprocess.Popen('/Users/yoshinori/Documents/OneDrive/codes/FlatReversi/Hamlet/dist/build/HamletProf/HamletProf +RTS -p -RTS', shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    print 'Engines started. Game starting:'

    won = play_a_game()

    print "Game Over! " + won + " won!\n"
    if won == "Black":
        black_won = black_won + 1
    else:
        white_won = white_won + 1

print "Result:\n"
print "Black wons: " + str(black_won) + ", White wons: " + str(white_won) + "\n"

