
def handle_case(case):
    processes = int(input().rstrip())
    computers = int(input().rstrip())
    left = []
    for i in range(processes):
        name, time, *deps = input().rstrip().split()
        time = int(time)
        left.append((name, set(deps), time))
    status = []

    counter = 0
    while status or left:
        counter += 1

        status = [(p, t-1) for (p,t) in status]
        done = set(p for (p,t) in status if t == 0)
        status = [(p, t) for (p,t) in status if t != 0]
        left = [(p, deps - done, t) for (p, deps, t) in left]

        if len(status) < computers:
            start = [(p, t) for (p, deps, t) in left if not deps]
            status = status + start[:computers - len(status)]
            left = [(p, deps, t) for (p, deps, t) in left if (p,t) not in status]


    print("{} {}".format(case + 1, counter - 1))
    

if __name__ == "__main__":
    ncases = int(input().strip())
    for case in range(ncases):
        handle_case(case)

