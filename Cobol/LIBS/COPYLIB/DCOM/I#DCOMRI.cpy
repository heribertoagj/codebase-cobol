      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRI                                      *
      * TAMANHO : 0250 BYTES                                           *
      * ARQUIVO : LAYOUT REDEFINICAO DO ARQUIVO RETORNO DO GATI        *
      *           (ARQUIVO GERAL E DETALHADO) PARA ATUALIZACAO DO CAMPO*
      *           RESULTADO DA TAB.TITULO ANALISE DE CREDITO           *
      *----------------------------------------------------------------*
      * CODIGOS DA ANALISE:                GATI DCOM                   *
PRM02 *                      LIBERADO     | 00 | 05 |                  *
PRM02 *                      BLOQUEADO    | 01 | 03 |                  *
PRM02 *                      ALERTA       | 02 | 01 |                  *
PRM02 *                      SEM ANALISE  | 09 | 04 |                  *
      *                      CPF INVALIDO | 98 | 02 |                  *
      *                      FALHA        | 99 | 02 |                  *
      ******************************************************************
DTS001* ALTERACOES.........:                                          *
DTS001*    - 05/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  DCOMRI-REG.

     ** DADOS DA ANALISE IRES - TIPO 001
           05  RI-DADOS-GERAL-IRES.
               10  RI-MAIOR-GRAU               PIC  9(002).
               10  FILLER                      PIC  X(248).

     ** DADOS DA ANALISE SUST - TIPO 002
           05  RI-DADOS-GERAL-SUST  REDEFINES  RI-DADOS-GERAL-IRES.
               10  RI-COD-MOTVO-SUSTACAO       PIC  X(003).
               10  RI-DESCR-MOTVO-SUSTACAO     PIC  X(050).
               10  RI-QTDE-CHEQ-SUSTADOS       PIC  9(009).
               10  FILLER                      PIC  X(188).

     ** DADOS DA ANALISE SLIG - TIPO 003
           05  RI-DADOS-GERAL-SLIG  REDEFINES  RI-DADOS-GERAL-IRES.
               10  RI-INDICE-LIQUIDEZ          PIC  9(003)V9(06).
               10  FILLER                      PIC  X(241).

     ** DADOS DA ANALISE CNSE - TIPO 004
           05  RI-DADOS-GERAL-CNSE   REDEFINES RI-DADOS-GERAL-IRES.
               10  RI-CODIGO-RECUSA            PIC  9(002).
               10  RI-DESCR-RECUSA             PIC  X(050).
               10  FILLER                      PIC  X(198).

     ** DADOS DA ANALISE RATING - TIPO 005
           05  RI-DADOS-GERAL-RATING REDEFINES RI-DADOS-GERAL-IRES.
               10  RI-CODIGO-RATING            PIC  X(003).
DTS001         10  RI-NNVEL-RTING              PIC  9(002).
DTS001         10  RI-HINIC-VGCIA-NVEL         PIC  X(026).
DTS001         10  FILLER                      PIC  X(219).

     ** DADOS DA ANALISE GRUP - TIPO 006
           05  RI-DADOS-GERAL-GRUP   REDEFINES RI-DADOS-GERAL-IRES.
               10  RI-GRUP-MENS                PIC  X(050).
               10  FILLER                      PIC  X(200).

     ** DADOS DA ANALISE CONCENTRACAO - TIPO 007
           05  RI-DADOS-GERAL-CONCENTR  REDEFINES   RI-DADOS-GERAL-IRES.
               10  RI-INDICE-CONCENTR          PIC  9(003)V9(06).
               10  RI-CONCENTR-MENS            PIC  X(050).
               10  RI-VR-PARAM                 PIC  9(016)V99.
               10  FILLER                      PIC  X(173).

     ** DADOS DA ANALISE CEP  - TIPO 008
           05  RI-DADOS-GERAL-CEP    REDEFINES RI-DADOS-GERAL-IRES.
               10  RI-CEP-MENS                 PIC  X(050).
               10  FILLER                      PIC  X(200).

     ** DADOS GERAL DA ANALISE  - TIPO 009
           05  RI-DADOS-GERAL        REDEFINES RI-DADOS-GERAL-IRES.
               10  RI-COD-ANALISE-DOCTO        PIC  X(020).
               10  FILLER                      PIC  X(230).

     ** DADOS IRES DETALHADO
           05  RI-DADOS-IRES-DET     REDEFINES RI-DADOS-GERAL-IRES.
               10  RI-COD-ANALISE-DOC          PIC  X(020).
               10  RI-DESCR-IMPEDIM            PIC  X(050).
               10  RI-QTDE-IMPEDIM             PIC  9(007).
               10  RI-DTA-PRIM-OCORR           PIC  9(008).
               10  RI-DTA-ULTI-OCORR           PIC  9(008).
               10  RI-VR-MINIMO                PIC  9(013)V99.
               10  RI-VR-MAXIMO                PIC  9(013)V99.
               10  RI-VR-TOTAL                 PIC  9(013)V99.
               10  RI-GRAU-IMPEDIM             PIC  9(001).
               10  FILLER                      PIC  X(111).
