      ******************************************************************
      * COPYBOOK  : BVVEW03A                                           *
      * DESCRICAO : CRRC041 - FECHAMENTO DE CONCILIAÇÃO                *
      * DATA      : 27/06/2023                                         *
      * AUTOR     : KATLIN CHAVES                                      *
      * EMPRESA   : IBM DO BRASIL                                      *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 45    BYTES                                        *
      ******************************************************************
      *
       01  BVVEW03A-REGISTRO.
           10 BVVEW03A-HEADER.
              15 BVVEW03A-H-INDR-TP-REG
                                      PIC  X(002).
              15 BVVEW03A-H-IDENTD-PART-PRINC
                                      PIC  X(008).
              15 BVVEW03A-H-IDENTD-PART-ADMTD
                                      PIC  X(008).
              15 BVVEW03A-H-DT-HR-ARQ
                                      PIC  X(019).
              15 FILLER               PIC  X(008).
           10 BVVEW03A-DETALHE REDEFINES BVVEW03A-HEADER.
              15 BVVEW03A-D-INDR-TP-REG
                                      PIC  X(002).
              15 BVVEW03A-D-INDR-TP-CONCC
                                      PIC  X(002).
              15 BVVEW03A-D-DT-REF
                                      PIC  X(010).
              15 BVVEW03A-D-NOM-ARQ
                                      PIC  X(031).
