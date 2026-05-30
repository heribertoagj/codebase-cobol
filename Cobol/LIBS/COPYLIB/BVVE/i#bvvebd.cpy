      *****************************************************************
      *   BVVE - AFILIACAO ELETRONICA DE ESTABELECIMENTOS             *
      *---------------------------------------------------------------*
      *   AREA     : I#BVVEBD                                         *
      *   TAMANHO  : 30                                               *
      *   OBJETIVO : ARQUIVO DE TELEFONE DO ESTABELECIMENTO           *
      *****************************************************************
      *
       01  BVVEBD-REGISTRO.
           05 BD-CCNPJ-CPF-ESTBL        PIC 9(009).
           05 BD-CFLIAL-CNPJ-ESTBL      PIC 9(005).
           05 BD-CCTRL-CNPJ-CPF         PIC 9(002).
           05 BD-CTPO-ENDER-AFLIA       PIC 9(001).
           05 BD-CTPO-FONE-ESTBL        PIC 9(001).
           05 BD-CSEQ-FONE-PPRIE        PIC 9(001).
           05 BD-CDDD-FONE-PPRIE        PIC X(004).
           05 BD-CFONE-PPRIE            PIC 9(011).
           05 FILLER                    PIC 9(006).
