# target : 
#  js   (and then make js)
#  text (and then make text)

TARGET = text
#TARGET = js

### do not edit below this line ###

#DEPENDENCY
DEP = depend_$(TARGET)

# source directories

SRC   = src/
LEM   = src_lem_library/
MEM   = src_memevents_litmus/
MODEL = src_model/

# js_of_ocaml directories

ifeq ($(JSLIBDIR), "")
  JSHOME = js_of_ocaml
  JSLIBDIR  = $(JSHOME)/lib
  JSCOMP    = $(JSHOME)/compiler/js_of_ocaml
  JSLIBNAME = $(JSLIBDIR)/js_of_ocaml.cma
  JSRUNTIME = $(JSHOME)/runtime/runtime.js
  PA_JS_CMO = $(JSLIBDIR)/syntax/pa_js.cmo
else
  JSCOMP    = $(JSBINDIR)/js_of_ocaml
  JSLIBNAME = $(JSLIBDIR)/js_of_ocaml.cma
  JSRUNTIME = $(JSLIBDIR)/runtime.js
  PA_JS_CMO = $(JSLIBDIR)/pa_js.cmo
endif

# preprocessor for the js version

PP = "camlp4o $(PA_JS_CMO)"

# OCAML for js

OCAMLDEP_JS = ocamldep -pp $(PP) -I $(SRC) -I $(LEM) -I $(MODEL) -I $(MEM)
OCAMLC_JS = ocamlfind ocamlc -annot -package lwt -pp $(PP) -I $(JSLIBDIR) -g -I $(SRC) -I $(LEM) -I $(MEM) -I $(MODEL)

# OCAML for text

OCAMLDEP_TEXT = ocamldep -I $(SRC) -I $(LEM) -I $(MODEL) -I $(MEM)
OCAMLC_TEXT = ocamlc -annot -g -I $(SRC) -I $(LEM) -I $(MEM) -I $(MODEL)
OCAMLOPT_TEXT = ocamlopt -I $(SRC) -I $(LEM) -I $(MEM) -I $(MODEL)

# OCAML, according to target

ifeq ($(TARGET), text) 
  OCAMLC = $(OCAMLC_TEXT)
  OCAMLDEP = $(OCAMLDEP_TEXT)
  OCAMLOPT = $(OCAMLOPT_TEXT)
else
  OCAMLC = $(OCAMLC_JS)
  OCAMLDEP = $(OCAMLDEP_JS)
endif

SRC_BASE = $(LEM)nat_num.ml $(LEM)pset.ml $(LEM)pmap.ml			\
           $(MODEL)MachineDefUtils.ml $(MODEL)MachineDefFreshIds.ml	\
           $(MODEL)MachineDefValue.ml $(MODEL)MachineDefTypes.ml	\
           $(MODEL)MachineDefInstructionSemantics.ml			\
           $(MODEL)MachineDefStorageSubsystem.ml			\
           $(MODEL)MachineDefThreadSubsystem.ml				\
           $(MODEL)MachineDefSystem.ml $(MEM)constrGen.ml		\
           $(MEM)debug.ml $(SRC)globals.ml $(MEM)mySet.ml $(MEM)pos.ml	\
           $(MEM)lexMisc.ml $(MEM)lexUtils.ml  $(MEM)misc.ml $(MEM)warn.ml \
			$(MEM)stateLexer.ml $(MEM)constr.ml $(MEM)stateParser.ml	\
           $(MEM)symbConstant.ml $(MEM)miscParser.ml			\
           $(MEM)logConstr.ml $(MEM)label.ml $(MEM)testHash.ml		\
           $(MEM)genParser.ml $(MEM)index.ml $(MEM)loader.ml		\
           $(MEM)location.ml $(MEM)MapString.ml $(MEM)stringMap.ml	\
           $(MEM)op.ml $(MEM)pseudo.ml $(MEM)tblRename.ml		\
           $(MEM)lexRename.ml $(MEM)PPCBase.ml                          \
	                      $(MEM)ARMBase.ml                          \
           $(MEM)rename.ml $(MEM)simpleDumper.ml $(MEM)StringSet.ml	\
           $(MEM)symbReg.ml $(MEM)symbValue.ml $(MEM)test.ml		\
           $(MEM)PPCLexer.ml $(MEM)splitter.ml $(MEM)PPCParser.ml	\
           $(MEM)ARMLexer.ml                   $(MEM)ARMParser.ml	\
           $(MEM)constraints.ml $(SRC)model_aux.ml $(SRC)pp.ml		\
           $(SRC)archExtra.ml $(SRC)interact.ml $(SRC)PPCArch.ml	\
                                                $(SRC)ARMArch.ml	\
           $(SRC)PPCIso.ml $(SRC)transitions.ml $(SRC)run.ml		\
           $(SRC)ARMIso.ml                                   		\
           $(SRC)version.ml $(MODEL)machine_version.ml $(SRC)top.ml $(SRC)types.ml


SRC_BASE_MLI = $(LEM)nat_num.mli $(LEM)pmap.mli $(LEM)pset.mli		\
              $(MEM)archBase.mli $(MEM)checks.mli $(MEM)constant.mli	\
              $(MEM)constrGen.mli $(MEM)debug.mli $(MEM)genParser.mli	\
              $(MEM)index.mli $(MEM)label.mli $(MEM)lexMisc.mli		\
              $(MEM)MapString.mli $(MEM)misc.mli $(MEM)miscParser.mli	\
              $(MEM)name.mli $(MEM)op.mli $(MEM)pos.mli			\
              $(MEM)relabel.mli $(MEM)rename.mli $(MEM)stringMap.mli	\
              $(MEM)StringSet.mli $(MEM)symbReg.mli			\
              $(MEM)symbValue.mli $(MEM)tblRename.mli $(MEM)value.mli	\
              $(MEM)PPCParser.mli $(MEM)stateParser.mli $(SRC)arch.mli	\
              $(MEM)ARMParser.mli                                     	\
              $(SRC)globals.mli $(SRC)interact.mli $(SRC)iso.mli	\
              $(SRC)pp.mli $(SRC)run.mli $(SRC)transitions.mli

ifeq ($(TARGET), text) 
  SOURCES =  $(SRC_BASE) $(SRC)main.ml
  SOURCES_MLI = $(SRC_BASE_MLI) $(MEM)warn.mli 
else
  SOURCES =  $(SRC_BASE) $(SRC)webppc.ml
  SOURCES_MLI = $(SRC_BASE_MLI)  
endif

CMO = $(patsubst %.ml, %.cmo, $(SOURCES)) 
CMX = $(patsubst %.ml, %.cmx, $(SOURCES)) 

# create links to the js files

link_js: 
	rm -f $(SRC)interact.ml $(SRC)interact.mli
	ln $(SRC)interact_js.ml $(SRC)interact.ml
	ln $(SRC)interact_js.mli $(SRC)interact.mli
#	ln $(SRC)top_text.ml $(SRC)top.ml

# create links to the text files

link_text: 
	rm -f $(SRC)interact.ml $(SRC)interact.mli 
	ln $(SRC)interact_text.ml $(SRC)interact.ml
	ln $(SRC)interact_text.mli $(SRC)interact.mli
#	ln $(SRC)top_text.ml $(SRC)top.ml

### main targets

js: link_js system.js

text: link_text system.opt

text_debug: link_text system.deb

#### rules

system.js: system.byte $(JSCOMP) $(JSRUNTIME)
	$(JSCOMP) -noruntime $(JSRUNTIME) system.byte $(OPTIONS)

system.byte: $(CMO)
	$(OCAMLC) -linkpkg -o $@ str.cma $(JSLIBNAME) $^

system.opt: $(CMX)
	$(OCAMLOPT) -o ppcmem str.cmxa $^

system.deb: $(CMO)
	$(OCAMLC) -o ppcmem str.cma $^

$(MEM)lexRename.ml: $(MEM)lexRename.mll
	ocamllex $<
$(MEM)lexUtils.ml: $(MEM)lexUtils.mll
	ocamllex $<
$(MEM)PPCLexer.ml: $(MEM)PPCLexer.mll
	ocamllex $<
$(MEM)ARMLexer.ml: $(MEM)ARMLexer.mll
	ocamllex $<
$(MEM)stateLexer.ml: $(MEM)stateLexer.mll
	ocamllex $<
$(MEM)splitter.ml: $(MEM)splitter.mll
	ocamllex $<
$(MEM)PPCParser.ml $(MEM)PPCParser.mli: $(MEM)PPCParser.mly
	ocamlyacc $(MEM)PPCParser.mly
$(MEM)ARMParser.ml $(MEM)ARMParser.mli: $(MEM)ARMParser.mly
	ocamlyacc $(MEM)ARMParser.mly
$(MEM)stateParser.ml $(MEM)stateParser.mli: $(MEM)stateParser.mly
	ocamlyacc $(MEM)stateParser.mly

# general rules

%.cmx: %.ml 
	$(OCAMLOPT) -c $< 

%.cmo: %.ml 
	$(OCAMLC) -c $< 

%.cmi: %.mli
	$(OCAMLC) -c $< 

# cleanup

clean:
	rm -f $(SRC)conf.ml $(MEM)*.cm[iox] $(SRC)*.cm[iox]		\
	$(LEM)*.cm[iox] $(MODEL)*.cm[io] $(MEM)lexRename.ml		\
	$(MEM)lexUtils.ml $(MEM)PPCLexer.ml $(MEM)stateLexer.ml		\
	$(MEM)splitter.ml $(MEM)PPCParser.ml $(MEM)PPCParser.mli	\
	                  $(MEM)ARMLexer.ml                      	\
	                  $(MEM)ARMParser.ml $(MEM)ARMParser.mli	\
	$(MEM)stateParser.ml $(MEM)stateParser.mli $(NAME).byte		\
	$(SRC)interact.ml $(SRC)interact.mli  
	rm -rf $(MODEL)*.cm[iox]   $(MODEL)*.o 


realclean:
	make clean
	rm -rf $(MEM)*.annot $(SRC)*.annot $(LEM)*.annot $(MODEL)*.annot
	rm -rf $(MEM)*.o     $(SRC)*.o     $(LEM)*.o     $(MODEL)*.o

# dependencies

.depend: $(DEP)

depend_js: link_js $(SOURCES) $(SOURCES_MLI)
	$(OCAMLDEP) $(SOURCES) $(SOURCES_MLI) > .depend

depend_text: link_text $(SOURCES) $(SOURCES_MLI)
	$(OCAMLDEP) $(SOURCES) $(SOURCES_MLI) > .depend

include .depend


######################  rebuild model and get model .ml files 

WEAKMEMORY= ../../../sem/WeakMemory

model:
	make -C $(WEAKMEMORY)/ppc-abstract-machine
	- chmod u+w src_model/*.ml*
	cp $(WEAKMEMORY)/ppc-abstract-machine/generated/*.ml* src_model
	chmod ugo-w src_model/*.ml*

######################  install in web dir

INSTALLDIR=~/public_html/ppcmem
INSTALLFILES=browse.css handler.js index.html stylesheet.css system.js url.js close.png jquery-1.6.1.js arrow-left.png arrow-right.png help.html ppc.css

jquery-1.6.1.js:
	wget -O $@ http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.js

install:
	make text
	cp $(INSTALLFILES) $(INSTALLDIR)
	make install_tarball

######################  add headers

SRC_BASE_NON_MEM_LEM = \
           $(MODEL)MachineDefUtils.ml $(MODEL)MachineDefFreshIds.ml	\
           $(MODEL)MachineDefValue.ml $(MODEL)MachineDefTypes.ml	\
           $(MODEL)MachineDefInstructionSemantics.ml			\
           $(MODEL)MachineDefStorageSubsystem.ml			\
           $(MODEL)MachineDefThreadSubsystem.ml				\
           $(MODEL)MachineDefSystem.ml                                  \
                                                 $(SRC)pp.ml		\
           $(SRC)archExtra.ml $(SRC)PPCArch.ml	 			\
                                                $(SRC)ARMArch.ml	\
           $(SRC)PPCIso.ml $(SRC)transitions.ml 			\
           $(SRC)ARMIso.ml                                   		\
           $(SRC)version.ml $(MODEL)machine_version.ml $(SRC)types.ml


SRC_BASE_MLI_NON_MEM_LEM = $(SRC)arch.mli	\
              $(SRC)globals.mli  $(SRC)iso.mli	\
              $(SRC)pp.mli $(SRC)transitions.mli

SRC_BASE_TEXT_OR_JS_REAL = \
	$(SRC)interact_js.ml   $(SRC)interact_text.ml   $(SRC)run_js.ml   $(SRC)run_text.ml   $(SRC)top_js.ml \
	$(SRC)interact_js.mli  $(SRC)interact_text.mli  $(SRC)run_js.mli  $(SRC)run_text.mli  $(SRC)top_text.ml


FILES = $(SRC_BASE_NON_MEM_LEM) $(SRC_BASE_TEXT_OR_JS_REAL) $(SRC_BASE_MLI_NON_MEM_LEM) 

headers:
	echo 'Adding headers to the files...'
	headache -c src/ppcmem_ha_cfg -h src/ppcmem_ha_hdr $(FILES)
	echo 'Done.'

######################   tarball

TARROOT=ppcmem-tarball
tarball:
	make realclean
	-chmod -R u+w $(TARROOT)/src-ppc
	rm -rf $(TARROOT)*
	mkdir $(TARROOT)
	cp -r $(SRC) $(LEM) $(MEM) $(MODEL) $(TARROOT)
	rm -rf $(TARROOT)/$(SRC)/.svn 
	rm -rf $(TARROOT)/$(LEM)/.svn 
	rm -rf $(TARROOT)/$(MEM)/.svn 
	rm -rf $(TARROOT)/$(MODEL)/.svn 
	cp browse.css close.png index.html stylesheet.css Makefile $(TARROOT)
	cp README_TARBALL $(TARROOT)/README
	cp LICENSE $(TARROOT)/
	cp VERSION $(TARROOT)/
	cp -R ~/public_html/ppcmem/src-ppc $(TARROOT)
	rm -rf $(TARROOT)/src-ppc/.svn 
	rm -rf $(TARROOT)/src-ppc/*/.svn 
	tar -zcvf $(TARROOT).tar.gz $(TARROOT)

install_tarball:
	make tarball
	cp $(TARROOT).tar.gz $(INSTALLDIR)
