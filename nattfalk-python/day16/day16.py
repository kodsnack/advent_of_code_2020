from itertools import chain

def solveA(parameters, my_ticket, nearby_tickets):
    all_values = set(list(chain.from_iterable(parameters.values())))
    error_rate = 0
    for nt in nearby_tickets:
        error_rate += sum(set(nt)-set(all_values))
    return error_rate

def solveB(parameters, my_ticket, nearby_tickets):
    all_values = set(list(chain.from_iterable(parameters.values())))
    valid_tickets = []
    valid_tickets.append(my_ticket)
    for nt in nearby_tickets:
        if sum(set(nt)-set(all_values)) > 0:
            # Invalid ticket
            continue 
        valid_tickets.append(nt)

    possible_field_names = {}
    for i in range(len(my_ticket)):
        field_values = [list(v)[i] for v in valid_tickets]

        for k, v in parameters.items():
            if k not in possible_field_names.keys():
                possible_field_names[k] = []
            if sum(set(field_values)-set(list(v))) == 0:
                possible_field_names[k].append(i)
    
    field_names = {}
    while(len(possible_field_names) > 0):
        k,v = next((k,v[0]) for k,v in possible_field_names.items() if len(v) == 1)
        field_names[k] = v
        possible_field_names.pop(k)
        for _,fields in possible_field_names.items():
            if v in fields:
                fields.remove(v)
    
    ticket_value = 1
    for i in [i for fn,i in field_names.items() if fn.startswith('departure')]:
        ticket_value *= my_ticket[i]

    return ticket_value

if __name__ == '__main__':
    parameters = {}
    my_ticket = []
    nearby_tickets = []

    input_type = 0
    with open('input.txt') as f:
        for l in f:
            l = l.strip()
            if not l:
                continue

            if l.startswith("your ticket"):
                input_type = 1
                continue
            elif l.startswith("nearby tickets"):
                input_type = 2
                continue

            if input_type == 0:
                param_parts = l.split(': ')
                range1 = param_parts[1].split(" or ")[0]
                range2 = param_parts[1].split(" or ")[1]
                
                r1 = int(range1.split("-")[0])
                r2 = int(range1.split("-")[1])
                parameters[param_parts[0]] = list(range(r1,r2+1))
                r1 = int(range2.split("-")[0])
                r2 = int(range2.split("-")[1])
                parameters[param_parts[0]].extend(list(range(r1,r2+1)))

            elif input_type == 1:
                my_ticket = [int(i) for i in l.split(",")]

            elif input_type == 2:
                nearby_tickets.append([int(i) for i in l.split(",")])

    print("Part 1:", solveA(parameters, my_ticket, nearby_tickets))
    print("Part 2:", solveB(parameters, my_ticket, nearby_tickets))
