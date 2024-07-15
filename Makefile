
gen:
	sicstus -l make_selfann.pl --goal "go,halt."

Call=app([a,b,c,d],[b],C)
CallOut=C
FILE=app.pl
lix:
	@echo "Specialising file $(FILE) for call $(Call)"
	sicstus -l lix_main.pl --goal "Call = $(Call),lix(Call, R1), portray_clause(':-'(Call,R1)),halt." >tests/$(FILE)
	@echo "Specialised file $(FILE)"
	cat tests/$(FILE)
	@echo "Running specialised file $(FILE)"
	sicstus -l tests/$(FILE) --goal "findall($(CallOut),$(Call),Bag), print(Bag),nl,halt."