import random as r 
r.seed(123)

def movimento(x, y, n):
    while 1:
        movimento  = r.choices(['N','S','L','O'], k = 1)[0]
        if movimento == 'N' and  y < n:
            y += 1
            return (x, y)
        elif movimento == 'S' and y> 1:
            y -= 1
            return (x, y)
        elif movimento == 'L' and x < n:
            x += 1
            return (x, y)
        elif movimento ==   'O' and x > 1:
            x -= 1
            return (x, y)


def cria_moedas(qtd_moedas, grade_map):
    moedas = set()
    i = 0
    while(i != qtd_moedas):
        pos  = (r.randint(1, grade_map), r.randint(1, grade_map))
        if pos[0] == 1  and pos[1] == 1:
            continue 
        elif pos in moedas:
            continue 
        else:
            i +=1 
            moedas.add(pos)
    return moedas



grade_map = 11
qtd_moedas = 4
vitoria= 0

perdeu = 0 


for j in range(1000):
    moedas = set(cria_moedas(qtd_moedas, grade_map))
    fantasmas  = [(1, grade_map), (grade_map, 1), (grade_map, grade_map)]
    pacman = (1,1)
    moedas_pegas = 0

    while(1):
        pacman = movimento(pacman[0],pacman[1], grade_map)
        if pacman in moedas:
            moedas.remove(pacman)
            moedas_pegas += 1
        for i in range(len(fantasmas)):
            fantasmas[i] = movimento(fantasmas[i][0], fantasmas[i][1], grade_map)
        if pacman in fantasmas:
            perdeu = perdeu +1 
            break
        elif moedas_pegas == qtd_moedas:
            vitoria = vitoria +1
            break

print(f"ganhou {vitoria}")