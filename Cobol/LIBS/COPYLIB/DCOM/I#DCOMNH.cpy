      *I#DCOMNH
      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNH *
      * CRIACAO   : 02/2007                                   *
      * DESCRICAO : BVVE - ARQUIVO DAS TRANSFERENCIAS DE AGEN-*
      *           : CIA E CONTA DE CREDITO - LOJISTAS         *
      *           : ROTATIVOPARCELADO (R=ROTATIVO, P=PARCELADO*
      * TAMANHO   : LRECL  032                                *
      *********************************************************
       01        DCOMNG-OPERACOES.
         05      DCOMNH-DADOS-ENTRADA.
           10    DCOMNH-AGE-CTA-LOJISTA.
             15  DCOMNH-BANCO-LOJISTA        PIC 9(03) COMP-3.
             15  DCOMNH-AGENCIA-LOJISTA      PIC 9(05) COMP-3.
             15  DCOMNH-CONTA-LOJISTA        PIC 9(13) COMP-3.
           10    DCOMNH-AGE-CTA-LOJISTA-ANT.
             15  DCOMNH-BANCO-LOJISTA-ANT    PIC 9(03) COMP-3.
             15  DCOMNH-AGENCIA-LOJISTA-ANT  PIC 9(05) COMP-3.
             15  DCOMNH-CONTA-LOJISTA-ANT    PIC 9(13) COMP-3.
         05      FILLER                      PIC X(08).
      *
