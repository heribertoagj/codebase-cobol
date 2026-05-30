      ***===========================================================***
      *** NOME INC                                     LENGTH=0650  ***
      *** I#CBON01                                                  ***
      ***-----------------------------------------------------------***
      *** DESCRICAO: ARQ. TITULOS PENDENTES/DBAU/PROTESTO/SAC-AVAL  ***
      ***            RESULTADO DA LEITURA DA F2D                    ***
FSYS19***===========================================================***
.     *        ALTERACAO, FEVEREIRO 2019 - MARCIO MAIA - FOURSYS      *
.     *   - INCLUSAO DOS CAMPOS DE VALOR MIN / MAX PARA PAGAMENTO     *
.     *   - PROJETO 2018-0355052-5-001                                *
FSYS19*---------------------------------------------------------------*
CNPJAN*---------------------------------------------------------------*
CNPJAN*        VERSAO 03, SETEMBRO 2025 - CARLOS SILVA / GFT          *
CNPJAN*   - PROJETO CNPJ ALFANUMERICO                                 *
CNPJAN*---------------------------------------------------------------*
       01  CBON01-REG.
           05  CBON01-PRODUTO                PIC  9(02)       COMP-3.
           05  CBON01-CNEGOC                 PIC  9(11)       COMP-3.
           05  CBON01-NOSNRO                 PIC  9(11)       COMP-3.
           05  CBON01-SEQUENCIA              PIC  9(03)       COMP-3.
           05  CBON01-CD-AUTO-AT             PIC  9(15)       COMP-3.
           05  CBON01-CLI-ESP                PIC  9(07)       COMP-3.
           05  CBON01-CD-BAN-DEP             PIC  9(03)       COMP-3.
           05  CBON01-CD-AGE-DEP             PIC  9(05)       COMP-3.
           05  CBON01-STATUS                 PIC  9(03)       COMP-3.
           05  CBON01-IND-SAC                PIC  9(01)       COMP-3.
           05  CBON01-IND-DBA                PIC  9(01)       COMP-3.
           05  CBON01-IND-PROT               PIC  9(01)       COMP-3.
           05  CBON01-IND-REAT               PIC  9(01)       COMP-3.
           05  CBON01-IND-REAP               PIC  9(01)       COMP-3.
           05  CBON01-IND-PGTO               PIC  9(01)       COMP-3.
           05  CBON01-IND-RAT                PIC  X(01).
           05  CBON01-IND-DTVEN              PIC  9(01)       COMP-3.
           05  CBON01-IND-MANUT              PIC  9(03)       COMP-3.
           05  CBON01-ARQ-SEQ-ORIGEM         PIC  9(02).
           05  CBON01-DATVEN                 PIC  9(09)       COMP-3.
           05  CBON01-DATEMI                 PIC  9(09)       COMP-3.
           05  CBON01-DATREA                 PIC  9(09)       COMP-3.
           05  CBON01-DATREG                 PIC  9(09)       COMP-3.
           05  CBON01-COD-NTAX               PIC  9(05)       COMP-3.
           05  CBON01-VALOR                  PIC  9(17)       COMP-3.
           05  CBON01-ABATIM                 PIC  9(17)       COMP-3.
           05  CBON01-IOF                    PIC  9(17)       COMP-3.
           05  CBON01-SEUNRO                 PIC  X(15).
           05  CBON01-CPARTI                 PIC  X(25).
           05  CBON01-SACADO                 PIC  X(40).
           05  CBON01-ENDERECO               PIC  X(40).
           05  CBON01-CEP                    PIC  9(05)       COMP-3.
           05  CBON01-CEP-SUFIXO             PIC  X(03).
           05  CBON01-TIPINS                 PIC  9(01)       COMP-3.
           05  CBON01-INSCRICAO              PIC  9(15)       COMP-3.
           05  CBON01-ST-CGC-CPF             PIC  9(01)       COMP-3.
           05  CBON01-DESC-COD1              PIC  X(01).
           05  CBON01-DESC-DAT1              PIC  9(09)       COMP-3.
           05  CBON01-DESC-VAL1              PIC  9(15)       COMP-3.
           05  CBON01-DESC-COD2              PIC  X(01).
           05  CBON01-DESC-DAT2              PIC  9(09)       COMP-3.
           05  CBON01-DESC-VAL2              PIC  9(15)       COMP-3.
           05  CBON01-DESC-COD3              PIC  X(01).
           05  CBON01-DESC-DAT3              PIC  9(09)       COMP-3.
           05  CBON01-DESC-VAL3              PIC  9(15)       COMP-3.
           05  CBON01-JUROS-COD              PIC  X(01).
           05  CBON01-JUROS-DAT              PIC  9(09)       COMP-3.
           05  CBON01-JUROS-VAL              PIC  9(17)       COMP-3.
           05  CBON01-MULTA-COD              PIC  X(01).
           05  CBON01-MULTA-DAT              PIC  9(09)       COMP-3.
           05  CBON01-MULTA-VAL              PIC  9(17)       COMP-3.
           05  CBON01-PROT-AUT               PIC  X(01).
           05  CBON01-NUMDIAS                PIC  9(03)       COMP-3.
           05  CBON01-BX-DEV-COD             PIC  X(01).
           05  CBON01-BX-DEV-DIA             PIC  9(03)       COMP-3.
           05  CBON01-COBCAR                 PIC  X(01).
           05  CBON01-TIPDOC                 PIC  X(01).
           05  CBON01-EMIBLOQ                PIC  X(01).
           05  CBON01-DISBLOQ                PIC  X(01).
           05  CBON01-ESPECIE                PIC  9(03)       COMP-3.
           05  CBON01-ACEITE                 PIC  X(01).
           05  CBON01-SUB-ORIGEM             PIC  9(05)       COMP-3.
           05  CBON01-TRT-PPLETA             PIC  9(05)       COMP-3.
           05  CBON01-COD-ORIGEM             PIC  9(03)       COMP-3.
           05  CBON01-AGE-OPER               PIC  9(05)       COMP-3.
           05  CBON01-NUM-EMPR               PIC  9(07)       COMP-3.
           05  CBON01-CIP                    PIC  9(03)       COMP-3.
           05  CBON01-SAC-AVAL               PIC  X(40).
           05  CBON01-TIPINS-AVAL            PIC  9(01)       COMP-3.
           05  CBON01-INSC-AVAL              PIC  9(15)       COMP-3.
           05  CBON01-ST-SAC-AVAL            PIC  9(01)       COMP-3.
           05  CBON01-BCO-DEB-AUT            PIC  9(03)       COMP-3.
           05  CBON01-AGE-DEB-AUT            PIC  9(05)       COMP-3.
           05  CBON01-DIG-AGE-DA             PIC  X(01).
           05  CBON01-GRP-DEB-AUT            PIC  9(03)       COMP-3.
           05  CBON01-SGRP-DEB-AUT           PIC  9(03)       COMP-3.
           05  CBON01-CCTA-DEB-AUT           PIC  9(13)       COMP-3.
           05  CBON01-DCTA-DEB-AUT           PIC  X(01).
           05  CBON01-DCTAE-DEB-AUT          PIC  X(01).
           05  CBON01-ENDER-AVISO            PIC  X(01).
           05  CBON01-IND-EMI-AVIS           PIC  X(01).
           05  CBON01-TIP-FOR-RET            PIC  9(03)       COMP-3.
           05  CBON01-TIP-MOT-RET            PIC  9(03)       COMP-3.
           05  CBON01-MOT-RET-SIS            PIC  9(03)       COMP-3.
           05  CBON01-DAT-DESAGENDA          PIC  9(09)       COMP-3.
           05  CBON01-PROT-DAT-INST          PIC  9(09)       COMP-3.
           05  CBON01-DAT-ENV-CART           PIC  9(09)       COMP-3.
           05  CBON01-DAT-SOL-SUST           PIC  9(09)       COMP-3.
           05  CBON01-DAT-EFE-SUST           PIC  9(09)       COMP-3.
           05  CBON01-DAT-CONS-SUST          PIC  9(09)       COMP-3.
           05  CBON01-IND-CON-SUST           PIC  9(01)       COMP-3.
           05  CBON01-NUM-CARTORIO           PIC  X(05).
           05  CBON01-NUM-PROTOCOL           PIC  X(15).
           05  CBON01-DEV-SUST               PIC  9(03)       COMP-3.
           05  CBON01-BAN-PROT               PIC  9(03)       COMP-3.
           05  CBON01-AGE-PROT               PIC  9(05)       COMP-3.
           05  CBON01-TIPO-ENDOSSO           PIC  X(01).
           05  CBON01-RESERVA.
               07  CBON01-TIT-LIVRE          PIC  X(39).
CNPJAN         07  CBON01-PASSAGEM           PIC  X(84).
CNPJAN         07  CBON01-CNPJAN.
CNPJAN             09  CBON01-RES1           PIC  X(24).
CNPJAN             09  CBON01-INSCRICAO-ST   PIC  X(15).
CNPJAN             09  CBON01-INSC-AVAL-ST   PIC  X(15).
CNPJAN             09  CBON01-QTDE-PARCELA   PIC  9(02).
           05  CBON01-AREA-ETAPA3            REDEFINES CBON01-RESERVA.
               07  CBON01-SEQ-PRACA          PIC  9(03)       COMP-3.
               07  CBON01-IDENT-COB-PROTE    PIC  X(01).
               07  CBON01-DATA-FIM-PROTE     PIC  9(09)       COMP-3.
               07  CBON01-DATA-ENCAR-PROTE   PIC  9(09)       COMP-3.
               07  CBON01-VALOR-ENCAR-PROTE  PIC  9(15)V9(02) COMP-3.
               07  CBON01-QDIA-ENCAR-PROTE   PIC  9(04)       COMP-3.
               07  CBON01-CTPO-COBR-ENCAR    PIC  X(01).
               07  CBON01-IND-TITLO-SOLUC    PIC  X(01).
               07  CBON01-IND-PAGTO-INDEVIDO PIC  X(01).
               07  CBON01-IND-SIT-IMPRE      PIC  X(01).
               07  CBON01-DATA-CARTA-SUST    PIC  9(09)       COMP-3.
               07  CBON01-HORA-SOLIC-SUST    PIC  9(11)       COMP-3.
               07  CBON01-ORIGEM-INST-PROTE  PIC  9(02)       COMP-3.
               07  CBON01-IND-CONSULTA-SUS   PIC  X(01).
               07  CBON01-VERSAO-REMESSA     PIC  9(06)       COMP-3.
               07  CBON01-CEP-SAC-AVAL       PIC  9(05)       COMP-3.
               07  CBON01-CEP-COMPL-SAC-AVAL PIC  9(03)       COMP-3.
               07  CBON01-ENDERECO-SAC-AVAL  PIC  X(40).
               07  CBON01-BANCO-PRACA        PIC  9(03)       COMP-3.
               07  CBON01-AGENCIA-PRACA      PIC  9(05)       COMP-3.
               07  CBON01-DATA-VERSAO-REM    PIC  9(09)       COMP-3.
FSYS19         07  CBON01-TP-PGTO-TITLO      PIC  9(02).
FSYS19         07  CBON01-TP-VL-MIN-PGTO     PIC  9(01).
FSYS19         07  CBON01-VL-MIN-PGTO        PIC  9(13)V99    COMP-3.
FSYS19         07  CBON01-PC-MIN-PGTO        REDEFINES
FSYS19             CBON01-VL-MIN-PGTO        PIC  9(10)V9(5)  COMP-3.
FSYS19         07  CBON01-TP-VL-MAX-PGTO     PIC  9(01).
FSYS19         07  CBON01-VL-MAX-PGTO        PIC  9(13)V99    COMP-3.
FSYS19         07  CBON01-PC-MAX-PGTO        REDEFINES
FSYS19             CBON01-VL-MAX-PGTO        PIC  9(10)V9(5)  COMP-3.
FSYS19         07  CBON01-UTILIZA-VLR-MIN    PIC  X(01).
FSYS19         07  CBON01-RES1               PIC  X(56).
