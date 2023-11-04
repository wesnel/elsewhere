.PHONY: clean
clean:
	eldev -p -dvQTC clean all

.PHONY: doctor
doctor:
	eldev -p -dvQTC doctor

.PHONY: lint
lint:
	eldev -p -dtTQC lint

.PHONY: compile
compile:
	eldev -p -dtTQC compile --set all --warnings-as-errors

.PHONY: test
test:
	eldev -dtTQC -U=undercover_coveralls_report test
