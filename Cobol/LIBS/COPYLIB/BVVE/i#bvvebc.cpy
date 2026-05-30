      *****************************************************************
      *   BVVE - AFILIACAO ELETRONICA DE ESTABELECIMENTOS             *
      *---------------------------------------------------------------*
      *   AREA     : I#BVVEBC                                         *
      *   TAMANHO  : 210                                              *
      *   OBJETIVO : ARQUIVO DE ENDERECO DO ESTABELECIMENTO           *
      *   BASEADO  : LAYOUT DA TABELA DE ENDR_ESTBL                   *
      *****************************************************************
      *
       01  BVVEBC-REGISTRO.
           05 BC-CCNPJ-CPF-ESTBL        PIC 9(009).
           05 BC-CFLIAL-CNPJ-ESTBL      PIC 9(005).
           05 BC-CCTRL-CNPJ-CPF         PIC 9(002).
           05 BC-CTPO-ENDER-AFLIA       PIC 9(001).
           05 BC-CCEP-COMPL             PIC 9(003).
           05 BC-CCEP                   PIC 9(005).
           05 BC-CSGL-UF                PIC X(002).
           05 BC-IMUN-ESTBL             PIC X(028).
           05 BC-EESTBL-AFLIA           PIC X(064).
           05 BC-EEMAIL-CNTAT-ESTBL     PIC X(050).
           05 BC-ICNTAT-ESTBL-AFLIA     PIC X(032).
           05 FILLER                    PIC 9(009).
