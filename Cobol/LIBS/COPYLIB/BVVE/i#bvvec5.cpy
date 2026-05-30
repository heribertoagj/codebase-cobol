      *===============================================================*
      *  ARQUIVO DE REMESSA/RETORNO PAG-FOR                           *
      *            BOOK = I#BVVEC5                TAMANHO = 500       *
      *===============================================================*
      *
       01 BVVEC5-REGISTRO                       PIC  X(500).
       01 BVVEC5-REG-HEADER  REDEFINES  BVVEC5-REGISTRO.
          05 BVVEC5-HE-IDENT-REG             PIC  9(01).
          05 BVVEC5-HE-IDENT-EMPR            PIC  9(08).
          05 BVVEC5-HE-TIP-INSCR-EMPR        PIC  9(01).
          05 BVVEC5-HE-CNPJ-CPF-EMPR         PIC  9(09).
          05 BVVEC5-HE-CNPJ-FILIAL-EMPR      PIC  9(04).
          05 BVVEC5-HE-CNPJ-CPF-CTR-EMPR     PIC  9(02).
          05 BVVEC5-HE-NOME-EMPR             PIC  X(40).
          05 BVVEC5-HE-TIP-SERVICO           PIC  9(02).
          05 BVVEC5-HE-ORIGEM-ARQ            PIC  9(01).
          05 BVVEC5-HE-NUM-REMESSA           PIC  9(05).
          05 BVVEC5-HE-NUM-RETORNO           PIC  9(05).
          05 BVVEC5-HE-DT-GRAV-ARQ           PIC  9(08).
          05 BVVEC5-HE-HR-GRAV-ARQ           PIC  9(06).
          05 BVVEC5-HE-DENS-GRAV-ARQ         PIC  X(05).
          05 BVVEC5-HE-UNI-GRAV-ARQ          PIC  X(03).
          05 BVVEC5-HE-IDENT-MOD-MICRO       PIC  X(05).
          05 BVVEC5-HE-TIP-PROCESS           PIC  9(01).
          05 BVVEC5-HE-FILLER-EMPRESA        PIC  X(74).
          05 BVVEC5-HE-FILLER-01             PIC  X(80).
          05 BVVEC5-HE-FILLER-02             PIC  X(217).
          05 BVVEC5-HE-NUM-DEBTO             PIC  9(09).
          05 BVVEC5-HE-FILLER-03             PIC  X(08).
          05 BVVEC5-HE-SEQ-REGISTRO          PIC  9(06).

       01 BVVEC5-REG-TRANSACAO  REDEFINES  BVVEC5-REGISTRO.
          05 BVVEC5-TS-IDENT-REG                PIC  9(01).
          05 BVVEC5-TS-TIP-INSCR-FORNEC         PIC  9(01).
          05 BVVEC5-TS-CNPJ-CPF-FORNEC          PIC  9(09).
          05 BVVEC5-TS-CNPJ-FILIAL-FORNEC       PIC  9(04).
          05 BVVEC5-TS-CNPJ-CPF-CTR-FORNEC      PIC  9(02).
          05 BVVEC5-TS-NOME-FORNEC              PIC  X(30).
          05 BVVEC5-TS-ENDER-FORNEC             PIC  X(40).
          05 BVVEC5-TS-CEP-FORNEC               PIC  9(05).
          05 BVVEC5-TS-CEP-COMPL                PIC  9(03).
          05 BVVEC5-TS-BANCO-FORNEC             PIC  9(03).
          05 BVVEC5-TS-AGENCIA-FORNEC           PIC  9(05).
          05 BVVEC5-TS-DIG-AG-FORNEC            PIC  X(01).
          05 BVVEC5-TS-CONTA-FORNEC             PIC  9(13).
          05 BVVEC5-TS-DIG-CTA-FORNEC           PIC  X(02).
          05 BVVEC5-TS-NUM-PAGAMENTO            PIC  X(16).
          05 BVVEC5-TS-CARTEIRA                 PIC  9(03).
          05 BVVEC5-TS-NOSSO-NUM                PIC  9(12).
          05 BVVEC5-TS-SEU-NUM                  PIC  X(15).
          05 BVVEC5-TS-DT-VENCTO                PIC  9(08).
          05 BVVEC5-TS-DT-EMIS-DOC              PIC  9(08).
          05 BVVEC5-TS-DT-LIM-DESC              PIC  9(08).
          05 BVVEC5-TS-ZERO                     PIC  9(01).
          05 BVVEC5-TS-FATOR-VENCTO             PIC  9(04).
          05 BVVEC5-TS-VALOR-DOC                PIC  9(08)V99.
          05 BVVEC5-TS-VALOR-PAGTO              PIC  9(13)V99.
          05 BVVEC5-TS-VALOR-DESC               PIC  9(13)V99.
          05 BVVEC5-TS-VALOR-ACRESCIMO          PIC  9(13)V99.
          05 BVVEC5-TS-TIP-DOC                  PIC  9(02).
          05 BVVEC5-TS-FILLER                   PIC  9(01).
          05 BVVEC5-TS-NUM-DOC                  PIC  9(09).
          05 BVVEC5-TS-SERIE-DOC                PIC  X(02).
          05 BVVEC5-TS-MODAL-PAGTO              PIC  9(02).
          05 BVVEC5-TS-DT-EFET-PAGTO            PIC  9(08).
          05 BVVEC5-TS-MOEDA                    PIC  X(03).
          05 BVVEC5-TS-SIT-AGENDA               PIC  9(02).
          05 BVVEC5-TS-FILLER-01                PIC  X(10).
          05 BVVEC5-TS-INFO-RETORNO  REDEFINES  BVVEC5-TS-FILLER-01.
             10 BVVEC5-TS-INFO-RETORNO-01       PIC  X(02).
             10 BVVEC5-TS-INFO-RETORNO-02       PIC  X(02).
             10 BVVEC5-TS-INFO-RETORNO-03       PIC  X(02).
             10 BVVEC5-TS-INFO-RETORNO-04       PIC  X(02).
             10 BVVEC5-TS-INFO-RETORNO-05       PIC  X(02).
          05 BVVEC5-TS-TIP-MOVTO                PIC  9(01).
          05 BVVEC5-TS-COD-MOVTO                PIC  9(02).
          05 BVVEC5-TS-HR-CONSULTA              PIC  X(04).
          05 BVVEC5-TS-SALDO-DISP               PIC  X(15).
          05 BVVEC5-TS-TAXA-PRE-FUN             PIC  X(15).
          05 BVVEC5-TS-FILLER-02                PIC  X(06).
          05 BVVEC5-TS-SACADOR                  PIC  X(40).
          05 BVVEC5-TS-FILLER-03                PIC  X(01).
          05 BVVEC5-TS-NIVEL-INFO               PIC  X(01).
          05 BVVEC5-TS-INFO-COMPL               PIC  X(40).
          05 BVVEC5-TS-COD-AREA                 PIC  9(02).
      *   SERA USADO PARA IDENTIFICAR A MENSAGEM ORIGEM
          05 BVVEC5-TS-FILLER-04                PIC  X(35).
          05 FILLER REDEFINES BVVEC5-TS-FILLER-04.
             07 BVVEC5-MSG-ORIGEM               PIC  9(02).
             07 BVVEC5-IND-CTRL                 PIC  X(01).
SVK          07 BVVEC5-RESERVADO                PIC  X(28).
IM1          07 BVVEC5-FLAG-RET                 PIC  X(01).
SVK          07 BVVEC5-BANDEIRA                 PIC  X(03).
          05 BVVEC5-TS-FILLER-05                PIC  X(22).
          05 BVVEC5-TS-COD-LANCTO               PIC  9(05).
          05 BVVEC5-TS-FILLER-06                PIC  X(01).
          05 BVVEC5-TS-TIP-CTA-FORNEC           PIC  9(01).
          05 BVVEC5-TS-CTA-COMPLEM              PIC  9(07).
          05 BVVEC5-TS-FILLER-07                PIC  X(08).
          05 BVVEC5-TS-SEQ-REGISTRO             PIC  9(06).

       01 BVVEC5-REG-TRAILLER  REDEFINES  BVVEC5-REGISTRO.
          05 BVVEC5-TR-IDENT-REG                PIC  9(01).
          05 BVVEC5-TR-QT-REG-GRAVADOS          PIC  9(06).
          05 BVVEC5-TR-TOTAL-VR-LANCTO          PIC  9(15)V99.
          05 BVVEC5-TR-FILLER                   PIC  X(470).
          05 BVVEC5-TR-SEQ-REGISTRO             PIC  9(06).
