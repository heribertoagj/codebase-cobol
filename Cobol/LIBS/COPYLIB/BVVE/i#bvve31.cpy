      ********************************************************
      ***   INC I#BVVE31 - LRECL: +3340                    ***
      ***   AREA DE ACESSO AO MODULO BVVE7021              ***
      ********************************************************

       01 BVVE31-INC.
          03 BVVE31-TPO-PROC                PIC  X(01).
          03 BVVE31-PGM-ORIGEM              PIC  X(08).
          03 BVVE31-NRO-PAGINA              PIC  9(05).
          03 BVVE31-QTE-OCO-PAG             PIC  9(02).
          03 BVVE31-QTE-OCO-RET             PIC  9(02).
          03 BVVE31-ULT-PAGINA              PIC  X(01).
          03 BVVE31-DEPARTAMENTO            PIC  9(05).
          03 BVVE31-ENTRADA.
             05 BVVE31-CCNPJ-CPF            PIC S9(09)  COMP-3.
             05 BVVE31-CFLIAL-CCNPJ         PIC S9(04)  COMP-3.
             05 BVVE31-CCTRL-CCNPJ          PIC S9(02)  COMP-3.
          03 FILLER                         PIC  X(26).
          03 BVVE31-RETORNO.
             05 BVVE31-COD-RETORNO          PIC  9(02).
             05 BVVE31-MSG-RETORNO          PIC  X(78).
             03 BVVE31-OCORR OCCURS 40 TIMES
                DEPENDING ON BVVE31-QTE-OCO-PAG.
                05 BVVE31-DESCR-CPO         PIC   X(18).
                05 BVVE31-CERRO-PREEN       PIC   9(02).
                05 BVVE31-RERRO-PREEN       PIC   X(60).


