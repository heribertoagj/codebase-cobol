      *================================================================*
      *  I#BVVEDY - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  AREA DE COMUNICACAO DO MODULO BVVE5000                        *
      *================================================================*

       01  BVVEDY-AREA.
           05 BVVEDY-ENTRADA.
              10 BVVEDY-NOME-ARQUIVO           PIC  X(008).
              10 BVVEDY-COD-BANCO.
                 15 BVVEDY-COD-BANCO-N         PIC  9(003).
              10 FILLER                        PIC  X(100).
           05 BVVEDY-SAIDA.
              10 BVVEDY-RET-CODIGO             PIC  9(002).
              10 BVVEDY-RET-MSG                PIC  X(079).
              10 FILLER                        PIC  X(100).
