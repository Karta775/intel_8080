#!/usr/bin/python3
# This is a really rough script that takes the provided opcodes.html file
# from emulator101.com and converts it into Rust match statements.

table = []

with open("opcodes.html") as fp:
    for line in fp:
        split = line.split("<td>")[1:]

        split[0] = split[0][3:5] # Clean up the first element (opcode)
        for i in range(1,5): # Clean up the extra </td>
            split[i] = split[i].replace("</td>", "")
        split[4] = split[4].replace("</tr>\n", "") # Clean up the final </tr>\n
        
        table.append(split)

for row in table:
    size = 1
    
    if row[2] != '':
        size = int(row[2])

    # Generate the lines of Rust code
    output = "0x"
    output += row[0] + " => { print_info(pc, "
    output += "&op_" + str(size) + ", "
    output += '"' + row[1] + '", '
    output += '"' + row[3] + '", '
    output += '"' + row[4] + '"); ' + str(size) + '},'
    print(output)
