      ******************************************************************
      * NOME DA INC - I#GFCTRN                                         *
      * DESCRICAO   - DADOS DE AGRUPAMENTO COM PRAZO VENCIDO           *
      * TAMANHO     - 150                                              *
      * DATA        - 05/2006                                          *
      * RESPONSAVEL - RENATO TAMANAHA  -  PROCWORK  - TEL 4152-4352    *
      ******************************************************************
      *
       01  GFCTRN-DADOS.
           03 GFCTRN-HEADER.
              10 GFCTRN-TIPO                 PIC 9(01).
              10 GFCTRN-DATA-BASE            PIC X(10).
              10 FILLER                      PIC X(139).
           03 GFCTRN-DETALHE REDEFINES GFCTRN-HEADER.
              10 FILLER                      PIC 9(01).
              10 GFCTRN-CAGPTO-CTA           PIC 9(03).
      *          CODIGO AGRUPAMENTO
              10 GFCTRN-DESC-TIPO-AGRUP      PIC X(25).
      *          TIPO AGRUPAMENTO
              10 GFCTRN-IDENT-AGRUPAMENTO    PIC X(20).
      *          IDENTIFICADOR AGRUPAMENTO
              10 GFCTRN-CSERVC-TARIF         PIC 9(05).
      *          TARIFA
              10 GFCTRN-CDEPDC-ORIGE-FLEXZ   PIC 9(05).
      *          DEPDC SOLICITANTE
              10 GFCTRN-HSOLTC-FLEXZ         PIC X(26).
      *          DATA SOLICITAÇ O
              10 GFCTRN-CEMPR-PAREC-FLEXZ    PIC 9(05).
      *           DEPDC RESPONSAVEL
              10 GFCTRN-HIDTFD-FLEXZ-AGPTO   PIC X(26).
      *          DATA LIMITE
              10 GFCTRN-DINIC-FLEXZ-AGPTO    PIC X(10).
      *          VIGENCIA FORMATO DD.MM.AAAA
              10 GFCTRN-DFIM-FLEXZ-AGPTO     PIC X(10).
      *          VIGENCIA FORMATO DD.MM.AAAA
              10 FILLER                      PIC X(14).
           03 GFCTRN-TRAILLER REDEFINES GFCTRN-HEADER.
              10 FILLER                      PIC 9(01).
              10 GFCTRN-ACU-LIDOS            PIC 9(08).
              10 GFCTRN-ACU-INDEF            PIC 9(08).
              10 FILLER                      PIC X(133).
