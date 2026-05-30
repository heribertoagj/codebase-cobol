      ******************************************************************
      * COPYBOOK  : BVVEW02A                                           *
      * DESCRICAO : CRRC039 - OPTIN                                    *
      * DATA      : 26/06/2023                                         *
      * AUTOR     : KATLIN CHAVES                                      *
      * EMPRESA   : IBM DO BRASIL                                      *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 157   BYTES                                        *
      ******************************************************************
      *
       01  BVVEW02A-REGISTRO.
           10 BVVEW02A-HEADER.
              15 BVVEW02A-H-INDR-TP-REG
                                      PIC  X(002).
              15 BVVEW02A-H-IDENTD-PART-PRINC
                                      PIC  X(008).
              15 BVVEW02A-H-IDENTD-PART-ADMTD
                                      PIC  X(008).
              15 BVVEW02A-H-DT-HR-ARQ
                                      PIC  X(019).
              15 FILLER               PIC  X(120).
           10 BVVEW02A-DETALHE REDEFINES BVVEW02A-HEADER.
              15 BVVEW02A-D-INDR-TP-REG
                                      PIC  X(002).
              15 BVVEW02A-D-CNPJ-BASE-CPFUSU
                                      PIC  X(014).
              15 BVVEW02A-D-CNPJ-CREDDR-SUB
                                      PIC  X(014).
              15 BVVEW02A-D-CNPJ-FINCDR
                                      PIC  X(014).
              15 BVVEW02A-D-COD-INSTDR-ARRAJ-PG
                                      PIC  X(003).
              15 BVVEW02A-D-DT-OPT-IN
                                      PIC  X(010).
              15 BVVEW02A-D-DT-INI-OPT-IN
                                      PIC  X(010).
              15 BVVEW02A-D-DT-FIM-OPT-IN
                                      PIC  X(010).
              15 BVVEW02A-D-IDENTD-CTRL-OPTIN
                                      PIC  X(019).
              15 BVVEW02A-D-IDENTD-CTRL-OPTOUT
                                      PIC  X(019).
              15 BVVEW02A-D-INDR-OPTN-FRCCNTRT
                                      PIC  X(001).
              15 BVVEW02A-D-INDR-SIT
                                      PIC  X(001).
              15 BVVEW02A-D-IDNTD-CTRLRQ-OPTIN
                                      PIC  X(020).
              15 BVVEW02A-D-IDNTD-CTRLRQ-OPTOUT
                                      PIC  X(020).
