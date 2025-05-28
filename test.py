data = "and, class, else, false, for, fun, if, nil, or, print, return, super, this, true, var, while"
lis = data.split(", ")

for item in lis:
    print(item.capitalize() + ",")
print()
print()
print()
for item in lis:
    print("Self::" + item.capitalize() + " => " + "\"" + item.upper() +"\"")

for item in lis:
    print("\"" + item + "\"" + "=> " + item.capitalize() + ",")