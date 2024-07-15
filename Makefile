
gen:
	sicstus -l make_selfann.pl --goal "go,halt."

Call=app([a,b,c,d],[b],C)
CallOut=C
FILE=app.pl
LCall=lix($(Call),Res)
lix:
	@echo "Specialising file $(FILE) for call $(Call) directly using Lix"
	sicstus -l lix_main.pl --goal "Call = $(Call),lix(Call, R1), portray_clause(':-'(Call,R1)),halt." >tests/$(FILE)
	@echo "Specialised file $(FILE):"
	cat tests/$(FILE)
	@echo "Running specialised file $(FILE)"
	sicstus -l tests/$(FILE) --goal "findall($(CallOut),$(Call),Bag), print(Bag),nl,halt."
lixgx:
	@echo "Generating GX $(FILE).gx for call $(Call) directly using Lix"
	sicstus -l lix_main.pl --goal "Call = $(LCall), lix(Call, R1), portray_clause(':-'(Call,R1)),halt." > tests/$(FILE).gx
	@echo "Generating extension file $(FILE).gx:"
	cat tests/$(FILE).gx
	@echo "Specialising using generating extension $(FILE).gx for call $(Call) directly using Lix"
	sicstus -l tests/$(File).gx --goal "$(LCall), portray_clause(':-'($(Call),Res)),halt." > tests/$(FILE).spec
	@echo "Specialised file $(FILE).spec:"
	cat tests/$(FILE).spec
	@echo "Running specialised file $(FILE).spec"
	sicstus -l tests/$(FILE).spec --goal "findall($(CallOut),$(Call),Bag), print(Bag),nl,halt."

CogCall=lix($(LCall),Res1)
cogen:
	sicstus -l lix_main.pl --goal "Call = $(CogCall),lix(Call, R1), portray_clause(':-'(Call,R1)),halt." > tests/$(FILE).cog
	@echo "Generated cogen $(FILE).cog:"
	cat tests/$(FILE).cog
