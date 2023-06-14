import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'add_transaction': 'ADD',
    'set_budget': 'SET',
    'savings_goal': 'GOAL',
    'spending_report': 'REPORT',
}

tokens = [
    'N',
    'ID',
    'ASIGN'
] + list(reserved.values())

t_ASIGN = r'^es$'

def t_N(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

t_ignore  = ' \t'

def t_error(t):
    print(":(")
    t.lexer.skip(1)

variables = {}
transacciones = []
presupuestos = {}
ahorros = 0

def p_resultado(t):
    'resultado : s'
    t[0] = t[1]

def p_asignacion(t):
    'resultado : ID ASIGN s'
    variables[t[1]] = t[3]

def p_expr_num(t):
    's : N'
    t[0] = t[1]

def p_expr_id(t):
    's : ID'
    try:
        t[0] = variables[t[1]]
    except LookupError:
        print("Error, por favor revise la sintaxis del comando ingresado")
        t[0] = 0

def p_add_transaction(t):
    's : ADD ID N'
    transacciones.append((t[2], t[3]))
    print("Se ha agregado una nueva transaccion")

def p_set_budget(t):
    's : SET ID N'
    presupuestos[t[2]] = t[3]
    print("Se ha establecido un presupuesto nuevo")

def p_savings_goal(t):
    's : GOAL N'
    global ahorros
    ahorros = t[2]
    print("Se ha establecido una nueva meta de ahorro")

def p_spending_report(t):
    's : REPORT'
    reporte = "Transaccion\tMonto\n--------------------\n"
    for transaccion, monto in transacciones:
        reporte += f"- {transaccion}\t\t{monto}\n"
    reporte += "--------------------\n"
    gastos_totales = sum([monto for _, monto in transacciones])
    reporte += f"Gastos totales en transacciones: {gastos_totales}\n"
    reporte += "Presupuestos actualizados despues de las transacciones > \n"
    for categoria, presupuesto in presupuestos.items():
        gastado = sum([monto for transaccion, monto in transacciones if transaccion == categoria])
        diferencia = presupuesto - gastado
        reporte += f"- {categoria}: {diferencia}\n"
    reporte += "--------------------\n"
    reporte += f"Meta de ahorro establecida en {ahorros}"
    t[0] = reporte

def p_error(t):
    print("Error, por favor revise la sintaxis del comando ingresado")

lexer = lex.lex()
parser = yacc.yacc()

while True:
    try:
        data = input()
    except EOFError:
        break
    result = parser.parse(data)
    if result is not None:
        print(result)
