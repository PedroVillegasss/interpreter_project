
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'ADD ASIGN GOAL ID N REPORT SETresultado : sresultado : ID ASIGN ss : Ns : IDs : ADD ID Ns : SET ID Ns : GOAL Ns : REPORT'
    
_lr_action_items = {'ID':([0,5,6,9,],[3,10,11,13,]),'N':([0,7,9,10,11,],[4,12,4,15,16,]),'ADD':([0,9,],[5,5,]),'SET':([0,9,],[6,6,]),'GOAL':([0,9,],[7,7,]),'REPORT':([0,9,],[8,8,]),'$end':([1,2,3,4,8,12,13,14,15,16,],[0,-1,-4,-3,-8,-7,-4,-2,-5,-6,]),'ASIGN':([3,],[9,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'resultado':([0,],[1,]),'s':([0,9,],[2,14,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> resultado","S'",1,None,None,None),
  ('resultado -> s','resultado',1,'p_resultado','main.py',41),
  ('resultado -> ID ASIGN s','resultado',3,'p_asignacion','main.py',45),
  ('s -> N','s',1,'p_expr_num','main.py',49),
  ('s -> ID','s',1,'p_expr_id','main.py',53),
  ('s -> ADD ID N','s',3,'p_add_transaction','main.py',61),
  ('s -> SET ID N','s',3,'p_set_budget','main.py',66),
  ('s -> GOAL N','s',2,'p_savings_goal','main.py',71),
  ('s -> REPORT','s',1,'p_spending_report','main.py',76),
]
