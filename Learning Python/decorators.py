# decorators

def print_it(fn):
    def show_result(a, b):
        print(f"{fn.__name__}({a},{b}) is = to {fn(a, b)}")
    return show_result


@print_it
def add(a, b):
    return a +  b


@print_it
def subt(a, b):
    print("This is subt")
    return a - b


@print_it
def mult(a, b):
    print("This is mult")
    return a * b


@print_it
def div(a, b):
    print("This is div")
    return a // b

# add(5, 4) # 9
# print("Let's go to the other one")
# subt(2, 3) # -1
div(10, 5)
© 2020 GitHub, Inc.