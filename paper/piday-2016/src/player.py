from retic import List,Tuple,Void,String,Int

class Player:

    def __init__(self:Player, name:Int, cards:List(Tuple(Int,Int)))->Void:
        self.name = name
        self.cards = cards

    def discard(self:Player)->Int:
        ....

    def choose_correct_stack(self:Player, stacks:List(List(Tuple(Int,Int))))->Int:
        ....

    def get_index_of_closest_stack(self:Player, cards:List(Tuple(Int,Int)), card:Tuple(Int,Int))->Int:
        ....
