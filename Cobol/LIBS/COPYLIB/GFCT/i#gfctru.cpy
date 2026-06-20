      *****************************************************************
      * NOME DA INC - I#GFCTRU                                        *
      * DESCRICAO   - ARQUIVO CLIEADES                                *
      * DATA        - MAIO/2006                                       *
ST2506* RESPONSAVEL - MARCELO HIRAI - PROCWORK - 55 POS.              *
      *****************************************************************
      *
       01  GFCTRU-ENTRADA.
           03 GFCTRU-AGENCIA                PIC S9(005)    COMP-3.
           03 GFCTRU-RAZAO                  PIC S9(005)    COMP-3.
           03 GFCTRU-CONTA                  PIC  9(007)    COMP-3.
           03 GFCTRU-CGC-CPF                PIC  9(009)    COMP-3.
           03 GFCTRU-FILIAL                 PIC  9(005)    COMP-3.
           03 GFCTRU-CONTR                  PIC  9(002).
           03 GFCTRU-POSTO-SERV             PIC  9(005)    COMP-3.
           03 GFCTRU-DT-ABERT               PIC  9(009)    COMP-3.
ST2506     03 FILLER                        PIC  X(012).
ST2506     03 GFCTRU-CGC-CPF-ST             PIC  X(009).
ST2506     03 GFCTRU-FILIAL-ST              PIC  X(004).
ST2506     03 GFCTRU-CONTR-ST               PIC  9(002).
