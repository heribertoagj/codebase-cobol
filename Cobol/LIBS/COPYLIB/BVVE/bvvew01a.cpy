      ******************************************************************
      * COPYBOOK  : BVVEW01A                                           *
      * DESCRICAO : CRRC034 - CONTRATOS                                *
      * DATA      : 26/06/2023                                         *
      * AUTOR     : JOSE AUGUSTO REGO                                  *
      * EMPRESA   : IBM DO BRASIL                                      *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 232  BYTES                                         *
      ******************************************************************
      *
       01  BVVEW01A-REGISTRO.
           10 BVVEW01A-HEADER.
              15 BVVEW01A-H-INDR-TP-REG
                                      PIC  X(002).
              15 BVVEW01A-H-IDENTD-PART-PRINC
                                      PIC  X(008).
              15 BVVEW01A-H-IDENTD-PART-ADMTD
                                      PIC  X(008).
              15 BVVEW01A-H-DT-HR-ARQ
                                      PIC  X(019).
              15 FILLER               PIC  X(195).
           10 BVVEW01A-DETALHE REDEFINES BVVEW01A-HEADER.
              15 BVVEW01A-D-INDR-TP-REG
                                      PIC  X(002).
              15 BVVEW01A-D-IDENTD-OP
                                      PIC  X(019).
              15 BVVEW01A-D-IDENTD-NEGC-RECBV
                                      PIC  X(040).
              15 BVVEW01A-D-INDR-TP-NEGC
                                      PIC  X(002).
              15 BVVEW01A-D-DT-VENC-OP
                                      PIC  X(010).
              15 BVVEW01A-D-VLR-TOT-LIM-SLD-D
                                      PIC  9(15)V99.
              15 BVVEW01A-D-VLR-GAR   PIC  9(15)V99.
              15 BVVEW01A-D-IND-ALC-CONTR-CD-SB
                                      PIC  X(001).
              15 BVVEW01A-D-IND-PERIOD-RECALC
                                      PIC  X(001).
              15 BVVEW01A-D-DIA-EXECC-RECALC
                                      PIC  X(002).
              15 BVVEW01A-D-INDR-SIT-OP
                                      PIC  X(001).
              15 BVVEW01A-D-CNPJ-CREDDR-SUB
                                      PIC  X(014).
              15 BVVEW01A-D-CNPJ-ER   PIC  X(014).
              15 BVVEW01A-D-CNPJ-BASE-CPFTIT
                                      PIC  X(014).
              15 BVVEW01A-D-CNPJ-CPFTIT-CT
                                      PIC  X(014).
              15 BVVEW01A-D-ISPBB-CORECBDR
                                      PIC  X(008).
              15 BVVEW01A-D-TP-CT     PIC  X(002).
              15 BVVEW01A-D-AG        PIC  9(004).
              15 BVVEW01A-D-CT        PIC  9(013).
              15 BVVEW01A-D-CT-PGTO   PIC  X(020).
              15 BVVEW01A-D-COD-INST-ARRAJ-PGTO
                                      PIC  X(003).
              15 BVVEW01A-D-CNPJ-BASE-CPFUSU
                                      PIC  X(014).
