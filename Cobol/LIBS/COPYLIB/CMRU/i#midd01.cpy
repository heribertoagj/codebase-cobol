      *----------------------------------------------------------------*
      *                  G & P       -       SET/2004                  *
      *----------------------------------------------------------------*
      *   I#MIDD01 - AREA DE LINKAGE PARA MODULOS MIDD9000 E MIDD9001  *
      *----------------------------------------------------------------*

       01  LNK-AREA-MIDD01.
           03  LNK-ENTRADA-MIDD01.
               05  LNK-BANCO-FUNC-X.
                   10  LNK-BANCO-FUNC       PIC  9(05).
               05  LNK-DEPTO-FUNC-X.
                   10  LNK-DEPTO-FUNC       PIC  9(06).
               05  LNK-COD-FUNC-X.
                   10  LNK-COD-FUNC         PIC  9(08).
               05  LNK-AGENCIA-PESQUISA-X.
                   10  LNK-AGENCIA-PESQUISA PIC  9(05).
           03  LNK-SAIDA-MIDD01.
               05  LNK-STATUS               PIC  9(02).
               05  LNK-RETORNO              PIC  9(02).
               05  LNK-MENSAGEM             PIC  X(79).
               05  LNK-MENSAGEM-R REDEFINES LNK-MENSAGEM.
                   10 LNK-CENTRO-CUSTO      PIC  X(04).
                   10 FILLER                PIC  X(75).

      *----------------------------------------------------------------*
