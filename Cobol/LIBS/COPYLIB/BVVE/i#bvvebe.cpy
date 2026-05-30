      *****************************************************************
      *   BVVE - AFILIACAO ELETRONICA DE ESTABELECIMENTOS             *
      *---------------------------------------------------------------*
      *   AREA     : I#BVVEBE                                         *
      *   TAMANHO  : 70                                               *
      *   OBJETIVO : ARQUIVO DE PROPRIETARIO                          *
      *   BASEADO  : LAYOUT DA TABELA DE PPRIE_ESTBL_AFLIA            *
      *****************************************************************
      *
       01  BVVEBE-REGISTRO.
           05 BE-CCNPJ-CPF-ESTBL        PIC 9(009).
           05 BE-CFLIAL-CNPJ-ESTBL      PIC 9(005).
           05 BE-CCTRL-CNPJ-CPF         PIC 9(002).
           05 BE-CCPF-PPRIE             PIC 9(009).
           05 BE-CTRL-CPF-PPRIE         PIC 9(002).
           05 BE-IPPRIE-ESTBL-AFLIA     PIC X(032).
           05 BE-DNASC-PPRIE            PIC 9(008).
           05 FILLER                    PIC 9(003).
