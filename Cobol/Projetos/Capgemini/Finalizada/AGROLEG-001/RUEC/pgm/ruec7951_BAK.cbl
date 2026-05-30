      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     RUEC7951.
       AUTHOR.         HERIBERTO GIANNASI.
      *================================================================*
      *                         7 C O M M                              *
      *----------------------------------------------------------------*
      *     PROGRAMA....: RUEC7951                                     *
      *     PROGRAMADOR.: HERIBERTO GIANNASI                           *
      *     ANALISTA....: HERIBERTO GIANNASI                           *
      *     DATA........: 01/2026                                      *
      *----------------------------------------------------------------*
      *     OBJETIVO....: MANTER CONTRATOS PARA RENEGOCIACAO           *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     RUECT797....: MANTER CONTRATOS PARA RENEGOCIACAO           *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *================================================================*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                             DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '** RUEC7951 - AREA DE WORKING **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DE ATRIBUTOS        ***'.
      *----------------------------------------------------------------*

       01  FILLER.
           03  WRK-ATRIBUTOS.

      *****************************************************************
      *    PROTEGIDO (NUMERICO OU ALFA)                               *
      *               -------- -- ----                                *
               05  WRK-225             PIC S9(008) COMP VALUE +225.
      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, ALFA                  *
      *                                         ----                  *
               05  WRK-49353           PIC S9(008) COMP VALUE +49353.
               05  FILLER              REDEFINES WRK-49353.
                   10 FILLER           PIC  X(002).
                   10 WRK-49353-X      PIC  X(002).
      *****************************************************************
      *    PROTEGIDO, BRILHANTE (NUMERICO OU ALFA)                    *
      *                          -------- -- ----                     *
               05  WRK-169             PIC S9(008) COMP VALUE +169.
      *****************************************************************
      *    NORMAL, DESPROT, CURSOR, ALFA                              *
      *                             ----                              *
               05  WRK-49345           PIC S9(008) COMP VALUE +49345.
      *****************************************************************
      *    NORMAL, DESPROT, ALFA                                      *
      *                                                               *
               05  WRK-193             PIC S9(008) COMP VALUE +193.
      *****************************************************************

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** VARIAVEIS AUXILIARES     ***'.
      *----------------------------------------------------------------*
       01 WRK-NUM-MAX-REG               PIC  9(02) COMP-3    VALUE 7.
       01 WRK-CBASE-LEGAL.
          05 FILLER                     PIC  9(03)        VALUE ZEROS.
          05 WRK-CBASE-LEGAL-N          PIC  9(02)        VALUE ZEROS.

       01 WRK-INDINCL                   PIC  9(02)  COMP-3 VALUE ZEROS.
       01 WRK-PAGINA                    PIC  9(02)  COMP-3 VALUE ZEROS.
       01 WRK-QTD-SEL                   PIC  9(01)  COMP-3 VALUE ZEROS.
       01 WRK-QTD-DEL                   PIC  9(01)  COMP-3 VALUE ZEROS.
       01 WRK-IND-SEL                   PIC  9(01)  COMP-3 VALUE ZEROS.
       01 WRK-IND-DEL                   PIC  9(01)  COMP-3 VALUE ZEROS.
       01 WRK-INDCD-INCL                PIC  X(01)         VALUE SPACES.

       01 WRK-INCONSIS                  PIC  X(001)    VALUE SPACES.
       01 WRK-MODULO                    PIC  X(008)    VALUE SPACES.
       01 WRK-AUX-MENSA                 PIC  X(079)    VALUE SPACES.

       01 IND-1                         PIC  9(003) COMP-3
                                                       VALUE ZEROS.
       01 WRK-FIM-CSR                   PIC  X(001)    VALUE SPACES.

       01 WRK-VALOR-TOTAL               PIC  9(15)V99 COMP-3
                                                       VALUE ZEROS.
       01 WRK-VALOR-TOTAL-AUX           PIC  9(15)V99  VALUE ZEROS.

       01 WRK-S9-09                     PIC +9(009)    VALUE ZEROS.
       01 FILLER                        REDEFINES WRK-S9-09.
          03 FILLER                     PIC  X(01).
          03 WRK-9-09                   PIC  9(009).

       77 WRK-QTD-CONTR                 PIC  9(02)     COMP-3
                                                       VALUE ZEROS.

       01 WRK-S9-15V2                   PIC +9(15)V99  VALUE ZEROS.
       01 FILLER REDEFINES WRK-S9-15V2.
          05 FILLER                     PIC  X(01).
          05 WRK-9-15V2                 PIC  9(15)V99.
       01 WRK-9-15V2-COMP               PIC  9(15)V99  COMP-3
                                                       VALUE ZEROS.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DO MODULO RUEC8500  ***'.
      *----------------------------------------------------------------*

           COPY 'I#RUEC08'.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA POOL5000         ***'.
      *----------------------------------------------------------------*

       01 WRK-CNPJ.
          05 WRK-CNPJ-CGC               PIC  ZZZZZZZZ9 VALUE ZEROS.
          05 FILLER                     PIC  X(01)     VALUE '/'.
          05 WRK-CNPJ-FILIAL            PIC  9(04)     VALUE ZEROS.
          05 FILLER                     PIC  X(01)     VALUE '-'.
          05 WRK-CNPJ-CTRL              PIC  9(02)     VALUE ZEROS.

       01 WRK-CPF.
          05 WRK-CPF-CGC                PIC  ZZZZZZZZ9 VALUE ZEROS.
          05 FILLER                     PIC  X(01)     VALUE '-'.
          05 WRK-CPF-CTRL               PIC  9(02)     VALUE ZEROS.

       01 WRK-GU                        PIC  X(004)    VALUE 'GU'.
       01 WRK-CHNG                      PIC  X(004)    VALUE 'CHNG'.
       01 WRK-ISRT                      PIC  X(004)    VALUE 'ISRT'.
       01 WRK-FUNCAO                    PIC  X(004)    VALUE SPACES.
       01 WRK-TELA                      PIC  X(008)    VALUE SPACES.
       01 WRK-NOVA-OPCAO                PIC  9(002)    VALUE ZEROS.
       01 WRK-OPCAO-ATUAL               PIC  9(002)    VALUE ZEROS.

       01 WRK-COD-USER-X.
          05 WRK-COD-USER               PIC  9(007)    VALUE ZEROS.

       01 WRK-COD-DEPTO.
          05 FILLER                     PIC  X(002)    VALUE SPACES.
          05 WRK-COD-DEPTO-N            PIC  9(004)    VALUE ZEROS.

       01 WRK-MENSAGEM.
          05 FILLER                     PIC  X(004)    VALUE SPACES.
          05 WRK-TRANSACAO              PIC  X(008)    VALUE SPACES.
          05 FILLER                     PIC  X(1988)   VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01 WRK-ERRO-BRAD0660.
          05 FILLER                     PIC  X(045)    VALUE
                '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
          05 FILLER                     PIC  X(012)    VALUE
                '- RET.CODE ='.
          05 WRK-RETURN-CODE            PIC  9(002)    VALUE ZEROS.
          05 FILLER                     PIC  X(011)    VALUE
                ' - LOCAL ='.
          05 WRK-LOCAL-ERRO             PIC  9(002)    VALUE ZEROS.
          05 FILLER                     PIC  X(002)    VALUE ' *'.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA RUECT797 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           03 FILLER                   PIC  X(018)         VALUE SPACES.
           03 WRK-I-PFK                PIC  X(001)         VALUE SPACES.
           03 WRK-I-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-I-NOPCAO             PIC  X(002)         VALUE SPACES.
           03 WRK-I-PAGINA             PIC  9(002)         VALUE ZEROS.
           03 WRK-I-PROX-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-I-ANTE-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-I-INDINCL            PIC  9(002)         VALUE ZEROS.
           03 WRK-I-INDEXCL            PIC  9(002)         VALUE ZEROS.
           03 WRK-I-FASE               PIC  9(002)         VALUE ZEROS.
           03 WRK-I-CTERM              PIC  X(008)         VALUE SPACES.
           03 WRK-I-FUNCAO             PIC  X(009)         VALUE SPACES.
           03 WRK-I-CEDULA             PIC  9(009)         VALUE ZEROS.
           03 WRK-I-DADOS.
              05 WRK-I-AGENCIA.
                 07 WRK-I-AGENCIA-N    PIC  9(005)         VALUE ZEROS.
              05 WRK-I-CONTA.
                 07 WRK-I-CONTA-N      PIC  9(08)          VALUE ZEROS.
              05 WRK-I-CNPJ-CPF        PIC  X(018)         VALUE SPACES.
              05 WRK-I-ITEM-LST        OCCURS 7.
                 07 WRK-I-ITEMLS       PIC  X(001)         VALUE SPACES.
                 07 WRK-I-REFBACEN     PIC  X(011)         VALUE SPACES.
                 07 WRK-I-CHVOPE.
                    10 WRK-I-CHVOPE-N PIC  9(009)          VALUE ZEROS.
                 07 WRK-I-SLDPRI.
                    10 WRK-I-SLDPRI-N PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-I-SLDENC.
                    10 WRK-I-SLDENC-N PIC  ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.
                 07 WRK-I-CODBAS.
                    10 WRK-I-CODBAS-N PIC  9(02)           VALUE ZEROS.
                 07 WRK-I-SLDREN.
                    10 WRK-I-SLDREN-N PIC   Z.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-SLDTOT.
                 07 WRK-I-SLDTOT-N     PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-MENSA           PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUECT797  - OUTPUT  ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05 WRK-O-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-O-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-O-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-O-NOPCAO             PIC  X(002)         VALUE SPACES.
           03 WRK-O-PAGINA             PIC  9(002)         VALUE ZEROS.
           03 WRK-O-PROX-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-O-ANTE-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-O-INDINCL            PIC  9(002)         VALUE ZEROS.
           03 WRK-O-INDEXCL            PIC  9(002)         VALUE ZEROS.
           03 WRK-O-FASE               PIC  9(002)         VALUE ZEROS.
           03 WRK-O-CTERM              PIC  X(008)         VALUE SPACES.
           03 WRK-O-FUNCAO             PIC  X(009)         VALUE SPACES.
           03 WRK-O-CEDULA             PIC  9(009)         VALUE ZEROS.
           03 WRK-O-DADOS.
              05 WRK-O-AGENCIA-ATTR    PIC  9(04) COMP     VALUE ZEROS.
              05 WRK-O-AGENCIA.
                 07 WRK-O-AGENCIA-N    PIC  9(005)         VALUE ZEROS.
              05 WRK-O-CONTA-ATTR      PIC  9(04) COMP     VALUE ZEROS.
              05 WRK-O-CONTA.
                 07 WRK-O-CONTA-N      PIC  9(08)          VALUE ZEROS.
              05 WRK-O-CNPJ-CPF-ATTR   PIC  9(04) COMP     VALUE ZEROS.
              05 WRK-O-CNPJ-CPF        PIC  X(018)         VALUE SPACES.
              05 WRK-O-ITEM-LST          OCCURS 7.
                 07 WRK-O-ITEMLS-ATTR    PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-ITEMLS         PIC  X(01)        VALUE SPACES.
                 07 WRK-O-REFBACEN-ATTR  PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-REFBACEN       PIC  X(11)        VALUE SPACES.
                 07 WRK-O-CHVOPE-ATTR    PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-CHVOPE.
                    10 WRK-O-CHVOPE-N    PIC  9(09)        VALUE ZEROS.
                 07 WRK-O-SLDPRI-ATTR    PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-SLDPRI.
                    10 WRK-O-SLDPRI-N    PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-O-SLDENC-ATTR    PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-SLDENC.
                    10 WRK-O-SLDENC-N  PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                 07 WRK-O-CODBAS-ATTR  PIC  9(04) COMP     VALUE ZEROS.
                 07 WRK-O-CODBAS.
                    10 WRK-O-CODBAS-N  PIC  9(002)         VALUE ZEROS.
                 07 WRK-O-SLDREN-ATTR  PIC  9(04) COMP     VALUE ZEROS.
                 07 WRK-O-SLDREN.
                    10 WRK-O-SLDREN-N  PIC  Z.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-SLDTOT-ATTR     PIC  9(04) COMP     VALUE ZEROS.
              05 WRK-O-SLDTOT.
                 07 WRK-O-SLDTOT-N     PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-MENSA           PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUECT797 - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUECT797.
           03 WRK-660-LL                 PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-LL-OUTPUT          PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-SENHAS             PIC S9(004) COMP  VALUE 0037.
           03 WRK-660-NOVOOPC            PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-PAGINA             PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-PRTELA             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-ANTELA             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-INDINCL            PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-FASE               PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-CTERM              PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-FUNCAO             PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-CEDULA             PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-AGENCIA            PIC S9(004) COMP  VALUE 0007.
           03 WRK-660-CONTA              PIC S9(004) COMP  VALUE 0010.
           03 WRK-660-CNPJCPF            PIC S9(004) COMP  VALUE 0020.
           03 WRK-660-LISTA              OCCURS 7.
              05 WRK-660-ITEMLS          PIC S9(004) COMP  VALUE 0003.
              05 WRK-660-REFBACEN        PIC S9(004) COMP  VALUE 0013.
              05 WRK-660-CHVOPE          PIC S9(004) COMP  VALUE 0011.
              05 WRK-660-SLDPRI          PIC S9(004) COMP  VALUE 0019.
              05 WRK-660-SLDENC          PIC S9(004) COMP  VALUE 0016.
              05 WRK-660-CODBAS          PIC S9(004) COMP  VALUE 0004.
              05 WRK-660-SLDREN          PIC S9(004) COMP  VALUE 0018.
           03 WRK-660-SLDTOT             PIC S9(004) COMP  VALUE 0020.
           03 WRK-660-MENSA              PIC S9(004) COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '* COPY I#RUEC00 - NAVEG-RURAL  *'.
      *----------------------------------------------------------------*

       COPY 'I#RUEC00'.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DE COMUNICACAO      ***'.
      *----------------------------------------------------------------*

       01 WRK-COMU-AREA.
          03 WRK-COMU-LL                PIC S9(004) COMP
                                                       VALUE ZEROS.
          03 WRK-COMU-ZZ                PIC S9(004) COMP
                                                       VALUE ZEROS.
          03 WRK-COMU-TRANCODE.
             10 WRK-COMU-TRANSACAO      PIC  X(008)    VALUE SPACES.
             10 FILLER                  PIC  X(006)    VALUE SPACES.
             10 WRK-COMU-PFK            PIC  X(001)    VALUE SPACES.
          03 WRK-COMU-SENHAS            PIC  X(037)    VALUE SPACES.
          03 WRK-COMU-OPCAO             PIC  9(002)    VALUE ZEROS.
          03 WRK-COMU-TERM              PIC  X(008)    VALUE SPACES.
          03 WRK-COMU-STATUS            PIC  X(002)    VALUE SPACES.
          03 WRK-COMU-MENSAGEM          PIC  X(079)    VALUE SPACES.
          03 WRK-COMU-PROX-TELA         PIC  X(008)    VALUE SPACES.
          03 WRK-COMU-VALOR-TOT         PIC S9(15)V9(02)
                                                       VALUE ZEROS.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA RUCA7602         ***'.
      *----------------------------------------------------------------*
       01 WRK-RUCA7602                  PIC  X(08)     VALUE 'RUCA7602'.
       01 WRK-AREA-7602.
      *COPY RUCAW602.
          03 WRK-W602-ENTRADA.
             05 WRK-W602-E-CCONTR            PIC 9(09) VALUE ZEROS.
*************** VARIAVEL PARA PERMITIR TESTES COM DEBUG
             05 WRK-W602-E-TPO-ACESSO        PIC X(01) VALUE SPACES.
*************** B=BATCH / OUTROS=IMS ONLINE
          03 WRK-W602-SAIDA.
             05 WRK-W602-S-SLD-DEVEDOR       PIC 9(13)V99 COMP-3
                                                       VALUE ZEROS.
             05 WRK-W602-S-SLD-ENCARGOS      PIC 9(13)V99 COMP-3
                                                       VALUE ZEROS.
             05 WRK-W602-S-SLD-TOTAL         PIC 9(13)V99 COMP-3
                                                       VALUE ZEROS.
             05 WRK-W602-S-NUM-PARCELAS      PIC 9(03)    COMP-3
                                                       VALUE ZEROS.
             05 WRK-W602-S-PARCELAS          OCCURS 120.
                10 WRK-W602-S-PCELA-NUM      PIC 9(03) VALUE ZEROS.
                10 WRK-W602-S-PCELA-DTEVENTO PIC X(10) VALUE SPACES.
                10 WRK-W602-S-PCELA-DTVCTO   PIC X(10) VALUE SPACES.
                10 WRK-W602-S-PCELA-VLRPRIN  PIC 9(13)V99 COMP-3
                                                       VALUE ZEROS.
                10 WRK-W602-S-PCELA-VLRENC   PIC 9(13)V99 COMP-3
                                                       VALUE ZEROS.
                10 WRK-W602-S-PCELA-CODLANC  PIC 9(04) VALUE ZEROS.
          03 WRK-W602-RETORNO.
             05 WRK-W602-RET-CODE            PIC 9(02)
                                                       VALUE ZEROS.
             05 WRK-W602-RET-MENSAGEM        PIC X(79)
                                                       VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** COMUNICACAO COM RUEC7952 ***'.
      *----------------------------------------------------------------*
       01 WRK-INPUT-7952.
          05 WRK-INP-7952-CJUNC         PIC  9(05)     VALUE ZEROS.
          05 WRK-INP-7952-CEDULA        PIC  9(09)     VALUE ZEROS.
          05 WRK-INP-7952-CREFBACEN     PIC  X(11)     VALUE SPACES.
          05 WRK-INP-7952-CHVOPE        PIC  9(09)     VALUE ZEROS.

       01 WRK-7952-AREA.
          05 WRK-7952-LL                PIC S9(004) COMP
                                                       VALUE ZEROS.
          05 WRK-7952-ZZ                PIC S9(004) COMP
                                                       VALUE ZEROS.
          05 WRK-7952-TRANCODE.
             10 WRK-7952-TRANSACAO      PIC  X(008)    VALUE SPACES.
             10 FILLER                  PIC  X(006)    VALUE SPACES.
             10 WRK-7952-PFK            PIC  X(001)    VALUE SPACES.
          05 WRK-7952-SENHAS            PIC  X(037)    VALUE SPACES.
          05 WRK-7952-OPCAO             PIC  9(002)    VALUE ZEROS.
          05 WRK-7952-CTERM             PIC  X(008)    VALUE SPACES.
          05 WRK-7952-STATUS            PIC  X(002)    VALUE SPACES.
          05 WRK-7952-MENSAGEM          PIC  X(079)    VALUE SPACES.
          05 WRK-7952-ANTE-TELA         PIC  X(008)    VALUE SPACES.
          05 WRK-7952-NAVEGACAO         PIC  X(300)    VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA BRAD7100         ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA PARA DB2            ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** INCLUDE SQLCA            ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB042
           END-EXEC.

           EXEC SQL
               INCLUDE RUECV007
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB00C
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB00D
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB00A
           END-EXEC.

           EXEC SQL
              DECLARE CSR01-RUECB00C SCROLL CURSOR FOR
              SELECT CREFT_RECOR
                   , VRENEG_CONTR_RURAL
                   , CBASE_LEGAL_RENEG
                   , VSDO_DVDOR_PRINC
                   , VTOT_ENCAR
                   , VTOT_JURO_MORA
                   , VTOT_MULTA
                FROM DB2PRD.TCDULA_TEMPR_RENEG
               WHERE CJUNC_DEPDC = :RUECB00C.CJUNC-DEPDC
                 AND CCDULA = :RUECB00C.CCDULA
           END-EXEC.

       01  WRK-DESPREZA                 PIC S9(09)  COMP-3  VALUE ZEROS.

           EXEC SQL
              DECLARE CSR02-RUECB00D CURSOR FOR
              SELECT CTPO_EVNTO_RURAL
                   , DEVNTO
                 FROM DB2PRD.TPCELA_TEMPR_RENEG
                WHERE CJUNC_DEPDC = :RUECB00D.CJUNC-DEPDC
                  AND CCDULA      = :RUECB00D.CCDULA
                  AND CREFT_RECOR = :RUECB00D.CREFT-RECOR
           END-EXEC.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '** RUEC7951 - FIM DA WORKING  **'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01 LNK-IO-PCB.
          05 LNK-IO-LTERM               PIC  X(008).
          05 FILLER                     PIC  X(002).
          05 LNK-IO-STATUS              PIC  X(002).
          05 FILLER                     PIC  X(012).
          05 LNK-IO-MODNAME             PIC  X(008).

       01 LNK-ALT-PCB.
          05 LNK-ALT-LTERM              PIC  X(008).
          05 FILLER                     PIC  X(002).
          05 LNK-ALT-STATUS             PIC  X(002).
          05 FILLER                     PIC  X(012).
          05 LNK-ALT-MODNAME            PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       0000-INICIAR                   SECTION.
      *----------------------------------------------------------------*

           ENTRY 'DLITCBL'            USING LNK-IO-PCB
                                            LNK-ALT-PCB.


           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       0000-10-POOL5000.
      *----------------------------------------------------------------*

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

           IF RETURN-CODE              EQUAL 04 OR 12
              PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO                          TO 0000-10-POOL5000.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'RUECT797'             TO WRK-TELA.

           MOVE WRK-I-PAGINA           TO WRK-PAGINA

           IF WRK-TRANSACAO            NOT EQUAL 'RUEC7951'
              PERFORM 1200-RECEBER-CONTROLE
           ELSE
              PERFORM 3000-PROCESSAR-RUECT797
           END-IF.

           IF WRK-TELA                 EQUAL 'RUECT797'
              PERFORM 7000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-ACESSAR-RUECB042           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CTERM            TO CTERM OF RUECB042.

           EXEC SQL
             SELECT WNAVG_CREDT_RURAL
               INTO :RUECB042.WNAVG-CREDT-RURAL
               FROM DB2PRD.NAVEGACAO_RURAL
              WHERE CTERM = :RUECB042.CTERM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB042'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1100'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB042'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1101'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WNAVG-CREDT-RURAL-TEXT OF RUECB042
                                       TO NAVEG-RURAL.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1200-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-SENHAS        TO WRK-I-SENHAS
                                          WRK-O-SENHAS.
           MOVE WRK-COMU-TERM          TO WRK-I-CTERM
                                          WRK-O-CTERM.
           MOVE 01                     TO WRK-I-PAGINA
                                          WRK-O-PAGINA.

           PERFORM 1100-ACESSAR-RUECB042.

           MOVE 'RUEC7951'             TO PROGRAMA-ANT.
           PERFORM 1222-UPDATE-RUECB042.

           MOVE WRK-COMU-PROX-TELA     TO WRK-I-PROX-TELA
                                          WRK-O-PROX-TELA.
           MOVE WRK-TRANSACAO          TO WRK-I-ANTE-TELA
                                          WRK-O-ANTE-TELA.

           MOVE WRK-COMU-PROX-TELA     TO WRK-I-PROX-TELA
                                          WRK-O-PROX-TELA.
           MOVE WRK-TRANSACAO          TO WRK-I-ANTE-TELA
                                          WRK-O-ANTE-TELA.
           IF WNAV-FUNCAO              OF NAVEGACAO-RURAL
              EQUAL 'A'
              MOVE 'ALTERACAO'         TO WRK-I-FUNCAO
                                          WRK-O-FUNCAO
           ELSE
              MOVE 'INCLUSAO'          TO WRK-I-FUNCAO
                                          WRK-O-FUNCAO
           END-IF.

           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO WRK-I-CEDULA
                                          WRK-O-CEDULA.

           MOVE ZEROS                  TO WRK-I-FASE

           PERFORM  1210-MONTAR-TELA.

           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE SPACES                 TO WRK-O-REFBACEN
                                          (WRK-INDINCL).

           MOVE SPACES                 TO WRK-O-REFBACEN
                                          (WRK-INDINCL).

           MOVE WRK-49345              TO WRK-O-REFBACEN-ATTR
                                          (WRK-INDINCL).

           MOVE 'INFORME OS DADOS E TECLE <ENTER>'
                                       TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1210-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*
           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO WRK-O-AGENCIA-N.

           MOVE CONTA                  OF NAVEGACAO-RURAL
                                       TO WRK-O-CONTA-N

           IF TIPO-PESSOA              OF NAVEGACAO-RURAL
                                       EQUAL 'F'
              MOVE NUMCGC              OF NAVEGACAO-RURAL
                                       TO WRK-CPF-CGC
              MOVE CTRL-CGC            OF NAVEGACAO-RURAL
                                       TO WRK-CPF-CTRL
              MOVE WRK-CPF             TO WRK-O-CNPJ-CPF
           ELSE
              MOVE NUMCGC              OF NAVEGACAO-RURAL
                                       TO WRK-CNPJ-CGC
              MOVE FILIAL              OF NAVEGACAO-RURAL
                                       TO WRK-CNPJ-FILIAL
              MOVE CTRL-CGC            OF NAVEGACAO-RURAL
                                       TO WRK-CNPJ-CTRL
              MOVE WRK-CNPJ            TO WRK-O-CNPJ-CPF
           END-IF.

           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE WRK-I-FASE             TO WRK-O-FASE.
           EVALUATE WRK-O-FASE
      *
      ****  TELA PARA CONSISTENCIA INCLUSAO COM CONTRATO RUFI
            WHEN 1
              PERFORM 1215-BLOQ-SELECAO

              IF WRK-INCONSIS          NOT EQUAL 'S'
                 PERFORM 1214-BLOQ-INCL

                 MOVE WRK-49345        TO WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)

              END-IF
      *
      ****  TELA PARA CONSISTENCIA INCLUSAO SEM CONTRATO RUFI
            WHEN 2
              PERFORM 1215-BLOQ-SELECAO

              IF WRK-INCONSIS          NOT EQUAL 'S'
                 PERFORM 1214-BLOQ-INCL

                 MOVE WRK-49345        TO WRK-O-SLDPRI-ATTR
                                          (WRK-INDINCL)
                 MOVE WRK-193          TO WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDENC-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDREN-ATTR
                                          (WRK-INDINCL)

                 MOVE SPACES           TO WRK-O-CHVOPE
                                          (WRK-INDINCL)
                 MOVE ZEROS            TO WRK-O-CODBAS-N(WRK-INDINCL)
                                          WRK-O-SLDPRI-N(WRK-INDINCL)
                                          WRK-O-SLDENC-N(WRK-INDINCL)
                                          WRK-O-SLDREN-N(WRK-INDINCL)
              END-IF
      *     TELA INCLUSAO COM CONTRATO RUFI
      *     TELA INCLUSAO SEM CONTRATO RUFI

            WHEN 4
            WHEN 5
              PERFORM 1215-BLOQ-SELECAO

              PERFORM 1214-BLOQ-INCL

              COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                         (WRK-I-SLDPRI(WRK-INDINCL))
              MOVE WRK-9-15V2-COMP      TO WRK-O-SLDPRI-N
                                           (WRK-INDINCL)

              COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                         (WRK-I-SLDREN(WRK-INDINCL))
              MOVE WRK-9-15V2-COMP      TO WRK-O-SLDREN-N
                                           (WRK-INDINCL)

              COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                         (WRK-I-SLDENC(WRK-INDINCL))
              MOVE WRK-9-15V2-COMP      TO WRK-O-SLDENC-N
                                           (WRK-INDINCL)

              MOVE WRK-I-CODBAS-N(WRK-INDINCL)
                                        TO WRK-O-CODBAS-N
                                           (WRK-INDINCL)
      *    FASE 0 -> INICIAL
            WHEN OTHER
              PERFORM 1213-LIMPAR-LISTA

              PERFORM 1300-CARREGAR-CONTRATOS

              IF WRK-INDINCL            NOT EQUAL ZEROS
                 PERFORM 1212-LIMPAR-INCLUSAO

                 MOVE WRK-49345         TO WRK-O-REFBACEN-ATTR
                                           (WRK-INDINCL)
              END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1212-LIMPAR-INCLUSAO            SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE SPACES                 TO WRK-O-ITEMLS(WRK-INDINCL)
                                          WRK-O-REFBACEN(WRK-INDINCL)
                                          WRK-O-CHVOPE(WRK-INDINCL)
                                          WRK-O-CODBAS(WRK-INDINCL)
                                          WRK-O-SLDPRI(WRK-INDINCL)
                                          WRK-O-SLDENC(WRK-INDINCL)
                                          WRK-O-SLDREN(WRK-INDINCL)

           MOVE SPACES                 TO WRK-I-ITEMLS(WRK-INDINCL)
                                          WRK-I-REFBACEN(WRK-INDINCL)
                                          WRK-I-CODBAS(WRK-INDINCL)
                                          WRK-I-CHVOPE(WRK-INDINCL)
                                          WRK-I-SLDPRI(WRK-INDINCL)
                                          WRK-I-SLDENC(WRK-INDINCL)
                                          WRK-I-SLDREN(WRK-INDINCL)

            MOVE WRK-225               TO WRK-O-ITEMLS-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-REFBACEN-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-CHVOPE-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDPRI-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDENC-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDREN-ATTR
                                          (WRK-INDINCL).

      *----------------------------------------------------------------*
       1212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1213-LIMPAR-LISTA               SECTION.
      *----------------------------------------------------------------*

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-NUM-MAX-REG
              MOVE SPACES              TO WRK-O-ITEMLS(IND-1)
                                          WRK-O-REFBACEN(IND-1)
                                          WRK-O-CODBAS(IND-1)
                                          WRK-O-CHVOPE(IND-1)
                                          WRK-O-SLDPRI(IND-1)
                                          WRK-O-SLDENC(IND-1)
                                          WRK-O-SLDREN(IND-1)
              MOVE WRK-225             TO WRK-O-ITEMLS-ATTR(IND-1)
                                          WRK-O-REFBACEN-ATTR(IND-1)
                                          WRK-O-CHVOPE-ATTR(IND-1)
                                          WRK-O-SLDPRI-ATTR(IND-1)
                                          WRK-O-SLDENC-ATTR(IND-1)
                                          WRK-O-CODBAS-ATTR(IND-1)
                                          WRK-O-SLDREN-ATTR(IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       1213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1214-BLOQ-INCL                  SECTION.
      *----------------------------------------------------------------*
           IF WRK-I-INDINCL            EQUAL ZEROS
              GO                       TO 1214-99-FIM
           END-IF.

           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE WRK-225                TO WRK-O-ITEMLS-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-REFBACEN-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDPRI-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDENC-ATTR
                                          (WRK-INDINCL)
                                          WRK-O-SLDREN-ATTR
                                          (WRK-INDINCL).

      *----------------------------------------------------------------*
       1214-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1215-BLOQ-SELECAO               SECTION.
      *----------------------------------------------------------------*
           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-NUM-MAX-REG
              IF  WRK-I-REFBACEN(IND-1) NOT EQUAL SPACES
              AND WRK-I-REFBACEN(IND-1) NOT EQUAL LOW-VALUES
                 MOVE WRK-225          TO WRK-O-ITEMLS-ATTR(IND-1)
              END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
       1215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1221-UPDATE-RUECV007            SECTION.
      *----------------------------------------------------------------*

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC OF RUECV007.
           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO CCDULA      OF RUECV007.

           EXEC SQL
             UPDATE DB2PRD.V01CEDULA_TEMPR
             SET CPROG = :RUECV007.CPROG
             WHERE CJUNC_DEPDC = :RUECV007.CJUNC-DEPDC
             AND CCDULA = :RUECV007.CCDULA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECV007'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'UPDATE'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1105'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
        1221-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1222-UPDATE-RUECB042            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CTERM            TO CTERM OF RUECB042.

           EXEC SQL
             UPDATE DB2PRD.NAVEGACAO_RURAL
                SET WNAVG_CREDT_RURAL = :RUECB042.WNAVG-CREDT-RURAL
              WHERE CTERM = :RUECB042.CTERM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB042'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'UPDATE'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1200'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1300-CARREGAR-CONTRATOS         SECTION.
      *----------------------------------------------------------------*
           MOVE ZEROS                  TO WRK-VALOR-TOTAL
                                          WRK-QTD-CONTR

           PERFORM 1310-ABRIR-CURSOR.

           PERFORM 1320-LER-CURSOR.

           PERFORM                     UNTIL WRK-FIM-CSR
                                       EQUAL 'S'
                                       OR WRK-QTD-CONTR
                                       GREATER WRK-NUM-MAX-REG

              PERFORM 1340-MOVER-TABELA

              PERFORM 1320-LER-CURSOR
           END-PERFORM.

           PERFORM 1330-FECHAR-CURSOR.

           IF WRK-QTD-CONTR            LESS WRK-NUM-MAX-REG
              MOVE WRK-QTD-CONTR       TO WRK-INDINCL

              ADD 1                    TO WRK-INDINCL
              MOVE WRK-INDINCL         TO WRK-O-INDINCL
                                          WRK-I-INDINCL
           ELSE
              MOVE ZEROS               TO WRK-O-INDINCL
                                          WRK-I-INDINCL
                                          WRK-INDINCL
           END-IF.

           MOVE WRK-169                TO WRK-O-SLDTOT-ATTR
           MOVE WRK-VALOR-TOTAL        TO WRK-O-SLDTOT-N.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1310-ABRIR-CURSOR            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RUECB00C.

           MOVE WRK-O-AGENCIA-N        TO CJUNC-DEPDC
                                       OF RUECB00C
           MOVE WRK-O-CEDULA           TO CCDULA
                                       OF RUECB00C

           EXEC SQL
              OPEN CSR01-RUECB00C
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00C'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'OPEN  '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1205'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR.
           MOVE ZEROS                  TO WRK-QTD-CONTR

           MOVE WRK-I-PAGINA           TO WRK-PAGINA
           COMPUTE WRK-DESPREZA        = ((WRK-PAGINA - 1) *
                                           WRK-NUM-MAX-REG) + 1.

      *----------------------------------------------------------------*
       1310-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1320-LER-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH ABSOLUTE :WRK-DESPREZA
               FROM CSR01-RUECB00C
               INTO :RUECB00C.CREFT-RECOR
                  , :RUECB00C.VRENEG-CONTR-RURAL
                  , :RUECB00C.CBASE-LEGAL-RENEG
                  , :RUECB00C.VSDO-DVDOR-PRINC
                  , :RUECB00C.VTOT-ENCAR
                  , :RUECB00C.VTOT-JURO-MORA
                  , :RUECB00C.VTOT-MULTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00C'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'FETCH '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1300'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR
           ELSE
               ADD 1                   TO WRK-DESPREZA
                                          WRK-QTD-CONTR
           END-IF.

      *----------------------------------------------------------------*
       1320-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1330-FECHAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR01-RUECB00C
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00C'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'CLOSE '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1305'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1330-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1340-MOVER-TABELA            SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-225             TO WRK-O-REFBACEN-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-CHVOPE-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-SLDPRI-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-SLDENC-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-CODBAS-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-SLDREN-ATTR
                                       (WRK-QTD-CONTR)

           MOVE WRK-193             TO WRK-O-ITEMLS-ATTR
                                       (WRK-QTD-CONTR)
           MOVE '_'                 TO WRK-O-ITEMLS
                                       (WRK-QTD-CONTR)
           INITIALIZE RUFIB002.
           MOVE CREFT-RECOR         OF RUECB00C
                                    TO WRK-O-REFBACEN
                                       (WRK-QTD-CONTR)
                                       WRK-I-REFBACEN
                                       (WRK-QTD-CONTR)
                                       CREFT-RECOR
                                    OF RUFIB002.

           MOVE 'N'                 TO WRK-INCONSIS

           PERFORM 3110-OBTER-CONTRATO-RUFI

           IF WRK-INCONSIS             EQUAL 'S'
              MOVE SPACES              TO WRK-O-CHVOPE
                                          (WRK-QTD-CONTR)
           ELSE
              MOVE CCONTR              OF RUFIB002
                                       TO WRK-S9-09
              MOVE WRK-9-09            TO WRK-O-CHVOPE-N
                                          (WRK-QTD-CONTR)
           END-IF

           MOVE VRENEG-CONTR-RURAL     OF RUECB00C
                                       TO WRK-S9-15V2.
           MOVE WRK-9-15V2             TO WRK-O-SLDREN-N
                                         (WRK-QTD-CONTR).

           ADD VRENEG-CONTR-RURAL      OF RUECB00C
                                       TO WRK-VALOR-TOTAL.

           MOVE CBASE-LEGAL-RENEG      OF RUECB00C
                                       TO WRK-CBASE-LEGAL
           MOVE WRK-CBASE-LEGAL-N      TO WRK-O-CODBAS-N
                                         (WRK-QTD-CONTR).

           MOVE VTOT-ENCAR             OF RUECB00C
                                       TO WRK-S9-15V2.
           MOVE WRK-9-15V2             TO WRK-O-SLDENC-N
                                          (WRK-QTD-CONTR).

           MOVE VTOT-JURO-MORA         OF RUECB00C
                                       TO WRK-S9-15V2.

           MOVE VSDO-DVDOR-PRINC       OF RUECB00C
                                       TO WRK-S9-15V2.
           MOVE WRK-9-15V2             TO WRK-O-SLDPRI-N
                                         (WRK-QTD-CONTR).

      *----------------------------------------------------------------*
       1340-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR-RUECT797         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT.

           PERFORM 3050-EFETUAR-INSPECT.
           PERFORM 1100-ACESSAR-RUECB042.
           PERFORM 3200-DEVOLVER-TELA

           EVALUATE TRUE

           WHEN WRK-I-PFK EQUAL '.'
               PERFORM 3100-TRATAR-ENTER

      * **     PF3
           WHEN WRK-I-PFK EQUAL 'C'
               PERFORM 3300-TRATAR-PF3

           WHEN WRK-I-PFK EQUAL '6'
               IF WRK-I-FASE          EQUAL '6'
                  PERFORM 3700-EXCLUIR-CONTRATO
               ELSE
                  PERFORM 3600-INCL-CONTR-PARCL
               END-IF

      * **     PF7
           WHEN WRK-I-PFK EQUAL '7'
               PERFORM 3400-PAGINA-ANTERIOR

           WHEN WRK-I-PFK EQUAL '8'
      * **     PF8
               PERFORM 3500-PROXIMA-PAGINA

      * **     PF11
           WHEN WRK-I-PFK EQUAL 'B'
               PERFORM 5100-TRATAR-PF11

      * **     PF INVALIDA
           WHEN OTHER
               MOVE 'PF INVALIDA'      TO WRK-O-MENSA
               MOVE ZEROS              TO WRK-I-FASE
               PERFORM 1210-MONTAR-TELA
           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3050-EFETUAR-INSPECT            SECTION.
      *----------------------------------------------------------------*

           IF WRK-I-AGENCIA            NOT EQUAL SPACES AND LOW-VALUES
              INSPECT WRK-I-AGENCIA    REPLACING LEADING ' ' BY '0'
           END-IF.

           IF WRK-I-CONTA              NOT EQUAL SPACES AND LOW-VALUES
              INSPECT WRK-I-CONTA      REPLACING LEADING ' ' BY '0'
           END-IF.

           IF WRK-I-CNPJ-CPF           NOT EQUAL SPACES AND LOW-VALUES
              INSPECT WRK-I-CNPJ-CPF   REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3100-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*
      *
      *  CASO O USUARIO TECLE <3NTER> NA FASE DE INCLUSAO A LINHA
      *  DE INCLUSAO TERA O VALOR APAGADO E PERMITIRA A ENTRADA
      *  DA REFERENCIA BACEN
           IF WRK-I-FASE               EQUAL 4 OR 5 OR 6
              MOVE ZEROS               TO WRK-I-FASE
              PERFORM 1210-MONTAR-TELA

              MOVE 'INFORME OS DADOS E TECLE <ENTER>'
                                       TO WRK-O-MENSA

              GO                       TO 3100-99-FIM
           END-IF.

           PERFORM 4000-CONSISTIR-AREA-ENTRADA

           IF WRK-INCONSIS             EQUAL 'S'
              PERFORM 1210-MONTAR-TELA
              GO                       TO 3100-99-FIM
           END-IF.


           IF WRK-I-FASE               EQUAL 4 OR 5
              PERFORM 1210-MONTAR-TELA
              MOVE 'TECLE PF6 PARA INSERIR OU <ENTER> ALTERAR'
                                    TO WRK-O-MENSA
              GO                    TO 3100-99-FIM
           END-IF

           IF WRK-QTD-SEL             EQUAL 1
              PERFORM 3120-PASSAR-CONTROLE
           END-IF

           IF WRK-QTD-DEL             EQUAL 1
              MOVE 6                    TO WRK-I-FASE
              MOVE 'TECLE PF6 PARA EXCLUIR OU <ENTER> PARA CANCELAR'
                                    TO WRK-O-MENSA
              GO                    TO 3100-99-FIM
           END-IF

           INITIALIZE RUFIB002.
           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE WRK-I-REFBACEN(WRK-INDINCL)
                                    TO WRK-O-REFBACEN
                                       (WRK-INDINCL)
                                       CREFT-RECOR
                                    OF RUFIB002.

           PERFORM 3110-OBTER-CONTRATO-RUFI.

      * CASO NAO ENCONTRAR O CONTRATO NO RUFI DEVEMOS PERMITIR O
      * PREENCHIMENTO DOS CAMPOS MANUALMENTE
           IF WRK-INCONSIS          EQUAL 'S'
              MOVE 'N'              TO WRK-INCONSIS
              MOVE 2                TO WRK-I-FASE

              PERFORM 1210-MONTAR-TELA

              MOVE 'TECLE PREENCHA INFORMACOES E TECLE <ENTER>'
                                    TO WRK-O-MENSA

              GO                    TO 3100-99-FIM
           END-IF.

           MOVE CCONTR              OF RUFIB002
                                    TO WRK-S9-09.
           MOVE WRK-9-09            TO WRK-O-CHVOPE-N
                                       (WRK-INDINCL).

           PERFORM 4200-TOTALIZAR-PARCELAS.

           IF WRK-INCONSIS             EQUAL 'S'
              MOVE 0                   TO WRK-I-FASE
              PERFORM 1210-MONTAR-TELA
              GO                       TO 3100-99-FIM
           END-IF


           MOVE WRK-W602-S-SLD-DEVEDOR TO WRK-O-SLDPRI-N
                                          (WRK-INDINCL).
      
           MOVE WRK-W602-S-SLD-ENCARGOS TO WRK-O-SLDENC-N
                                          (WRK-INDINCL).
      
           MOVE WRK-W602-S-SLD-TOTAL   TO WRK-O-SLDREN-N
                                          (WRK-INDINCL).

           MOVE WRK-49345              TO WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL).

           MOVE ZEROS                  TO WRK-O-CODBAS-N
                                          (WRK-INDINCL).


           MOVE 1                      TO WRK-I-FASE
           PERFORM 1210-MONTAR-TELA
           MOVE 'INFORME O CODIGO DE BASE LEGAL'
                                       TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3110-OBTER-CONTRATO-RUFI        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              SELECT CCONTR
                INTO :RUFIB002.CCONTR
                FROM DB2PRD.CONTRATO_RURAL
               WHERE CREFT_RECOR = :RUFIB002.CREFT-RECOR
               FETCH FIRST ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUFIB002'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1400'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'S'                 TO WRK-INCONSIS
              GO TO 3110-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3120-PASSAR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CEDULA           TO WRK-INP-7952-CEDULA.

           MOVE WRK-I-AGENCIA-N        TO WRK-INP-7952-CJUNC.

           MOVE WRK-I-REFBACEN(WRK-IND-SEL)
                                       TO WRK-INP-7952-CREFBACEN.

           MOVE WRK-I-CHVOPE-N(WRK-IND-SEL)
                                       TO WRK-INP-7952-CHVOPE.

           INITIALIZE WRK-7952-AREA.
           MOVE LENGTH OF WRK-7952-AREA TO WRK-7952-LL
           MOVE ZEROS                  TO  WRK-7952-ZZ
           MOVE '.'                    TO  WRK-7952-PFK
           MOVE WRK-I-SENHAS           TO  WRK-7952-SENHAS
           MOVE ZEROS                  TO  WRK-7952-OPCAO
           MOVE WRK-I-CTERM            TO  WRK-7952-CTERM
           MOVE SPACES                 TO  WRK-7952-MENSAGEM
                                           WRK-7952-ANTE-TELA
                                           WRK-7952-TRANSACAO
           MOVE WRK-INPUT-7952         TO  WRK-7952-NAVEGACAO.

           MOVE WRK-7952-AREA          TO  WRK-MENSAGEM.
           MOVE WRK-CHNG               TO  WRK-FUNCAO.
           MOVE 'RUEC7952'             TO  WRK-TELA.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-SENHAS           TO WRK-O-SENHAS.
           MOVE WRK-I-PAGINA           TO WRK-O-PAGINA.
           MOVE WRK-I-PROX-TELA        TO WRK-O-PROX-TELA.
           MOVE WRK-I-ANTE-TELA        TO WRK-O-ANTE-TELA.
           MOVE WRK-I-INDINCL          TO WRK-O-INDINCL.
           MOVE WRK-I-FASE             TO WRK-O-FASE.
           MOVE WRK-I-CTERM            TO WRK-O-CTERM.
           MOVE WRK-I-FUNCAO           TO WRK-O-FUNCAO.
           MOVE WRK-I-CEDULA           TO WRK-O-CEDULA.
           MOVE WRK-I-AGENCIA          TO WRK-O-AGENCIA.
           MOVE WRK-I-CONTA            TO WRK-O-CONTA.
           MOVE WRK-I-CNPJ-CPF         TO WRK-O-CNPJ-CPF.

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-NUM-MAX-REG

              MOVE WRK-I-ITEMLS(IND-1) TO WRK-O-ITEMLS(IND-1)
              MOVE WRK-I-REFBACEN(IND-1)
                                       TO WRK-O-REFBACEN(IND-1)
              MOVE WRK-I-CHVOPE(IND-1) TO WRK-O-CHVOPE(IND-1)
              MOVE WRK-I-SLDPRI(IND-1) TO WRK-O-SLDPRI(IND-1)
              MOVE WRK-I-SLDENC(IND-1) TO WRK-O-SLDENC(IND-1)
              MOVE WRK-I-CODBAS(IND-1) TO WRK-O-CODBAS(IND-1)
              MOVE WRK-I-SLDREN(IND-1) TO WRK-O-SLDREN(IND-1)

              IF WRK-O-CHVOPE-N(IND-1) NOT NUMERIC
              OR WRK-O-CHVOPE-N(IND-1) EQUAL ZEROS
              OR IND-1                 EQUAL WRK-I-INDINCL
                 MOVE WRK-225          TO WRK-O-ITEMLS-ATTR(IND-1)
              ELSE
                 MOVE WRK-49345        TO WRK-O-ITEMLS-ATTR(IND-1)
              END-IF

              MOVE WRK-225             TO WRK-O-REFBACEN-ATTR(IND-1)
                                          WRK-O-CHVOPE-ATTR(IND-1)
                                          WRK-O-SLDPRI-ATTR(IND-1)
                                          WRK-O-SLDENC-ATTR(IND-1)
                                          WRK-O-CODBAS-ATTR(IND-1)
                                          WRK-O-SLDREN-ATTR(IND-1)
           END-PERFORM.

           MOVE WRK-225                TO WRK-O-SLDTOT-ATTR
           MOVE WRK-I-SLDTOT           TO WRK-O-SLDTOT.
           MOVE WRK-I-MENSA            TO WRK-O-MENSA.

      *----------------------------------------------------------------*
        3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3300-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE LENGTH OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE ZEROS                  TO WRK-COMU-ZZ.
           MOVE 'RUEC7951'             TO WRK-COMU-TRANSACAO.
           MOVE '.'                    TO WRK-COMU-PFK.
           MOVE WRK-I-SENHAS           TO WRK-COMU-SENHAS.
           MOVE ZEROS                  TO WRK-COMU-OPCAO.
           MOVE WRK-I-CTERM            TO WRK-COMU-TERM.
           MOVE SPACES                 TO WRK-COMU-STATUS
                                          WRK-COMU-MENSAGEM.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

           IF WNAV-FUNCAO              OF NAVEGACAO-RURAL
                                       EQUAL 'A'
              MOVE 'RUEC8710'          TO WRK-TELA
           ELSE
              MOVE 'RUEC7000'          TO WRK-TELA
           END-IF.

           MOVE 'RUEC7030'             TO CPROG OF RUECV007.

           PERFORM 1221-UPDATE-RUECV007.

      *----------------------------------------------------------------*
        3300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3400-PAGINA-ANTERIOR            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-PAGINA           TO WRK-PAGINA

           IF WRK-I-PAGINA             GREATER 1
              COMPUTE WRK-PAGINA     = WRK-PAGINA - 1
              MOVE WRK-PAGINA          TO WRK-I-PAGINA
           ELSE
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM
              GO                       TO 3400-99-FIM
           END-IF.

           PERFORM 1213-LIMPAR-LISTA.

           MOVE WRK-I-AGENCIA-N        TO WRK-O-AGENCIA-N
           MOVE WRK-I-CEDULA           TO WRK-O-CEDULA
           PERFORM 1300-CARREGAR-CONTRATOS.

      *----------------------------------------------------------------*
       3400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3500-PROXIMA-PAGINA             SECTION.
      *----------------------------------------------------------------*

           IF WRK-I-INDINCL            NOT EQUAL ZEROS
              MOVE 'NAO EXISTE PROXIMA PAGINA'
                                       TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM
              GO                       TO 3500-99-FIM

           ELSE
              MOVE WRK-I-PAGINA        TO WRK-PAGINA

              COMPUTE WRK-PAGINA     = WRK-PAGINA + 1
              MOVE WRK-PAGINA          TO WRK-I-PAGINA
                                          WRK-O-PAGINA
           END-IF.

           PERFORM 1213-LIMPAR-LISTA.

           MOVE WRK-I-AGENCIA-N        TO WRK-O-AGENCIA-N
           MOVE WRK-I-CEDULA           TO WRK-O-CEDULA
           PERFORM 1300-CARREGAR-CONTRATOS.

      *----------------------------------------------------------------*
       3500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3600-INCL-CONTR-PARCL           SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-I-INDINCL          TO WRK-INDINCL

           IF WRK-I-FASE               NOT EQUAL 4 AND 5
              MOVE ZEROS               TO WRK-I-FASE
              PERFORM 1210-MONTAR-TELA
              GO                       TO 3600-99-FIM
           END-IF

           PERFORM 3610-INCLUIR-CONTRATO.

           IF WRK-INCONSIS             EQUAL 'S'
              GO                       TO 3600-99-FIM
           END-IF

           IF WRK-I-FASE              EQUAL 4
               MOVE WRK-I-CHVOPE(WRK-INDINCL)
                                      TO WRK-O-CHVOPE(WRK-INDINCL)

               PERFORM 4200-TOTALIZAR-PARCELAS
      
               PERFORM                 VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-W602-S-NUM-PARCELAS
                                       OR WRK-INCONSIS
                                       EQUAL 'S'
                 PERFORM 3620-INCLUIR-PARCELAS
              END-PERFORM
      
              IF WRK-INCONSIS             EQUAL 'S'
                 GO                       TO 3600-99-FIM
              END-IF
           END-IF.

           MOVE 0                      TO WRK-I-FASE
                                          WRK-I-INDINCL
                                          WRK-O-INDINCL


           PERFORM 1210-MONTAR-TELA.

           MOVE 'CONTRATO INCLUIDO COM SUCESSO'
                                       TO WRK-O-MENSA.
                                   
      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3610-INCLUIR-CONTRATO           SECTION.
      *----------------------------------------------------------------*

           PERFORM 3611-MOVER-CONTRATO.

           EXEC SQL
              INSERT INTO DB2PRD.TCDULA_TEMPR_RENEG
                   (  CJUNC_DEPDC,
                      CCDULA,
                      CREFT_RECOR,
                      VRENEG_CONTR_RURAL,
                      CBASE_LEGAL_RENEG,
                      VSDO_DVDOR_PRINC,
                      VTOT_ENCAR,
                      VTOT_JURO_MORA,
                      VTOT_MULTA,
                      CINDCD_TPO_RENEG,
                      CUSUAR_INCL,
                      HINCL,
                      CUSUAR_MANUT,
                      HMANUT_REG)
              VALUES (:RUECB00C.CJUNC-DEPDC,
                      :RUECB00C.CCDULA,
                      :RUECB00C.CREFT-RECOR,
                      :RUECB00C.VRENEG-CONTR-RURAL,
                      :RUECB00C.CBASE-LEGAL-RENEG,
                      :RUECB00C.VSDO-DVDOR-PRINC,
                      :RUECB00C.VTOT-ENCAR,
                      :RUECB00C.VTOT-JURO-MORA,
                      :RUECB00C.VTOT-MULTA,
                      :RUECB00C.CINDCD-TPO-RENEG,
                      '000000000',
                      CURRENT_TIMESTAMP,
                      NULL,
                      NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00D'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1405'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL -803
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'CONTRATO JA INCLUIDO PARA CEDULA'
                                       TO WRK-O-MENSA
              MOVE ZEROS               TO WRK-I-FASE
              PERFORM 1210-MONTAR-TELA
              GO                       TO 3610-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3611-MOVER-CONTRATO             SECTION.
      *----------------------------------------------------------------*
           INITIALIZE RUECB00C

           MOVE WRK-I-AGENCIA-N       TO CJUNC-DEPDC
                                      OF RUECB00C

           MOVE WRK-I-CEDULA          TO CCDULA
                                      OF RUECB00C

           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE WRK-I-REFBACEN(WRK-INDINCL)
                                      TO CREFT-RECOR
                                      OF RUECB00C

           COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                      (WRK-I-SLDREN-N(WRK-INDINCL))
           MOVE WRK-9-15V2-COMP       TO VRENEG-CONTR-RURAL
                                      OF RUECB00C

           COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                      (WRK-I-SLDPRI-N(WRK-INDINCL))
           MOVE WRK-9-15V2-COMP       TO VSDO-DVDOR-PRINC
                                      OF RUECB00C

           COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                      (WRK-I-SLDENC-N(WRK-INDINCL))
           MOVE WRK-9-15V2-COMP       TO VTOT-ENCAR
                                      OF RUECB00C
           MOVE ZEROS                 TO VTOT-JURO-MORA
                                      OF RUECB00C
                                         VTOT-MULTA
                                      OF RUECB00C

           INITIALIZE WRK-CBASE-LEGAL
           MOVE WRK-I-CODBAS-N(WRK-INDINCL)
                                      TO WRK-CBASE-LEGAL-N
           MOVE WRK-CBASE-LEGAL       TO CBASE-LEGAL-RENEG
                                      OF RUECB00C

           MOVE SPACES                TO CINDCD-TPO-RENEG,
                                      OF RUECB00C.

      *----------------------------------------------------------------*
       3611-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3620-INCLUIR-PARCELAS           SECTION.
      *----------------------------------------------------------------*
      
           PERFORM 3621-MOVER-PARCELAS
      
           EXEC SQL
              INSERT INTO DB2PRD.TPCELA_TEMPR_RENEG
                    ( CJUNC_DEPDC,
                      CCDULA,
                      CREFT_RECOR,
                      CTPO_EVNTO_RURAL,
                      DEVNTO,
                      NPCELA_CONTR,
                      DVCTO,
                      VSDO_DVDOR_PRINC,
                      VTOT_ENCAR,
                      VTOT_JURO_MORA,
                      VTOT_MULTA,
                      CINDCD_PCEL_RENEG,
                      CUSUAR_INCL,
                      HINCL_REG,
                      CUSUAR_MANUT,
                      HMANUT_REG,
                      CBCO,
                      CAG_BCRIA)
              VALUES (:RUECB00D.CJUNC-DEPDC,
                      :RUECB00D.CCDULA,
                      :RUECB00D.CREFT-RECOR,
                      :RUECB00D.CTPO-EVNTO-RURAL,
                      :RUECB00D.DEVNTO,
                      :RUECB00D.NPCELA-CONTR,
                      :RUECB00D.DVCTO,
                      :RUECB00D.VSDO-DVDOR-PRINC,
                      :RUECB00D.VTOT-ENCAR,
                      :RUECB00D.VTOT-JURO-MORA,
                      :RUECB00D.VTOT-MULTA,
                      :RUECB00D.CINDCD-PCEL-RENEG,
                      'RUEC7951',
                      CURRENT_TIMESTAMP,
                      NULL,
                      NULL,
                      :RUECB00D.CBCO,
                      :RUECB00D.CAG-BCRIA)
           END-EXEC.
      
           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00D'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1406'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.
      
           IF SQLCODE                  EQUAL -803
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'PARCELA DO CONTRATO JA INCLUIDO PARA CEDULA'
                                       TO WRK-O-MENSA
              MOVE ZEROS               TO WRK-I-FASE
              PERFORM 1210-MONTAR-TELA
              GO                       TO 3620-99-FIM
           END-IF.
      
      *----------------------------------------------------------------*
       3620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3621-MOVER-PARCELAS             SECTION.
      *----------------------------------------------------------------*
           INITIALIZE RUECB00D
      
      
           MOVE WRK-I-AGENCIA-N       TO CJUNC-DEPDC
                                      OF RUECB00D
                                         CAG-BCRIA
                                      OF RUECB00D
      
           MOVE WRK-I-CEDULA          TO CCDULA
                                      OF RUECB00D
      
           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE WRK-I-REFBACEN(WRK-INDINCL)
                                      TO CREFT-RECOR
                                      OF RUECB00D
      
           MOVE ZEROS                 TO CTPO-EVNTO-RURAL
                                      OF RUECB00D
      
           MOVE WRK-W602-S-PCELA-DTEVENTO(IND-1)
                                       TO DEVNTO
                                       OF RUECB00D
                                          DVCTO
                                       OF RUECB00D
      
           MOVE WRK-W602-S-PCELA-NUM(IND-1)
                                       TO NPCELA-CONTR
                                       OF RUECB00D
      
           MOVE ZEROS                  TO CBCO
                                       OF RUECB00D
      
           MOVE WRK-W602-S-PCELA-VLRPRIN(IND-1)
                                       TO VSDO-DVDOR-PRINC
                                       OF RUECB00D
      
           MOVE WRK-W602-S-PCELA-VLRENC(IND-1)
                                       TO VTOT-ENCAR
                                       OF RUECB00D
                                         VTOT-JURO-MORA
                                       OF RUECB00D
                                         VTOT-MULTA
                                       OF RUECB00D
      
           MOVE 'S'                    TO CINDCD-PCEL-RENEG
                                       OF RUECB00D.
            
      *----------------------------------------------------------------*
       3621-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3700-EXCLUIR-CONTRATO           SECTION.
      *----------------------------------------------------------------*
           
           IF WRK-I-FASE                NOT EQUAL 6
              MOVE ZEROS                TO WRK-I-FASE
              GO                        TO 3700-99-FIM
           END-IF.

           PERFORM 3710-EXCLUIR-PARCELAS.

           PERFORM 3720-EXCLUIR-CONTRATO.

           PERFORM 1210-MONTAR-TELA.

           MOVE 'CONTRATO EXCLUIDO COM SUCESSO'
                                       TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3710-EXCLUIR-PARCELAS
      *----------------------------------------------------------------*
           
           PERFORM 3711-ABRIR-CSR02.

           PERFORM 3712-LER-CSR02.

           PERFORM                     UNTIL WRK-FIM-CSR02
                                       EQUAL 'S'

              PERFORM 3714-EXEC-EXCLUIR-PARCELA

              PERFORM 3712-LER-CSR02
           END-PERFORM.

           PERFORM 3713-FECHAR-CSR02.

      *----------------------------------------------------------------*
       3710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3711-ABRIR-CSR02                SECTION.
      *----------------------------------------------------------------*
           
           INITIALIZE RUECB00D

           MOVE WRK-I-AGENCIA-N        TO CJUNC-DEPDC 
                                       OF RUECB00D
           MOVE WRK-I-CEDULA           TO CCDULA
                                       OF RUECB00D
           MOVE WRK-I-REFBACEN(WRK-I-INDEXCL)
                                       TO CREFT-RECOR 
                                       OF RUECB00D
           EXEC SQL 
              OPEN CSR02-RUECB00D
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00D'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'OPEN  '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '3711'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR02.
      
      *----------------------------------------------------------------*
       3711-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3712-LER-CSR02                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH CSR02-RUECB00D
               INTO :RUECB00D.CTPO-EVNTO-RURAL
                  , :RUECB00D.DEVNTO                               
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00D'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'FETCH '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '3712'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL 100
              MOVE 'S'                 TO WRK-FIM-CSR02
           END-IF.

      *----------------------------------------------------------------*
       3712-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3713-FECHAR-CSR02               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR02-RUECB00D
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00D'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'CLOSE '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '3713'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3713-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3714-EXEC-EXCLUIR-PARCELA       SECTION.
      *----------------------------------------------------------------*
           
           EXEC SQL
              DELETE FROM DB2PRD.TPCELA_TEMPR_RENEG
               WHERE CJUNC_DEPDC = :RUECB00D.CJUNC-DEPDC 
                 AND CCDULA      = :RUECB00D.CCDULA      
                 AND CREFT_RECOR = :RUECB00D.CREFT-RECOR 
                 AND CTPO_EVNTO_RURAL 
                                 = :RUECB00D.CTPO-EVNTO-RURAL
                 AND DEVNTO      = :RUECB00D.DEVNTO                               
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00D'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'DELETE'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '3714'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3714-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4000-CONSISTIR-AREA-ENTRADA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-O-MENSA

           MOVE 'N'                    TO WRK-INCONSIS
                                          WRK-INDCD-INCL.
           MOVE ZEROS                  TO WRK-QTD-SEL
                                          WRK-IND-SEL.

           IF WRK-I-FASE               EQUAL 2
      * CONSISTE INCLUSAO SEM CONTRATO
              PERFORM 4140-CONSISTIR-INCL-SCONTR
           ELSE
      * CONSISTE INCLUSAO COM CONTRATO
              IF WRK-I-FASE               EQUAL 1
                 PERFORM 4130-CONSISTIR-INCL-CONTR
              ELSE
      * CONSISTE ENTRADA INICIAL
                 PERFORM 4100-CONSISTIR-CAMPOS

              END-IF
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4100-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-INDINCL          TO WRK-INDINCL

           IF  WRK-I-INDINCL           NOT EQUAL ZEROS
               IF  WRK-I-REFBACEN(WRK-INDINCL)
                                       NOT EQUAL LOW-VALUES
               AND WRK-I-REFBACEN(WRK-INDINCL)
                                       NOT EQUAL SPACES
                   MOVE 'S'            TO WRK-INDCD-INCL
                   GO                  TO 4100-99-FIM
               END-IF
           END-IF.

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-NUM-MAX-REG
              IF WRK-I-ITEMLS(IND-1)   EQUAL 'X'
                 ADD 1                 TO WRK-QTD-SEL
                 MOVE IND-1            TO WRK-IND-SEL
              END-IF
           END-PERFORM.

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-NUM-MAX-REG
              IF WRK-I-ITEMLS(IND-1)   EQUAL 'E'
                 ADD 1                 TO WRK-QTD-DEL
                 MOVE IND-1            TO WRK-IND-DEL
              END-IF
           END-PERFORM.

           IF  WRK-QTD-SEL             EQUAL ZEROS
           AND WRK-QTD-DEL             EQUAL ZEROS
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'SELECIONE LINHA P/ CONSULTA, EXCLUSAO OU INFORME 
      -       'REFBACEN'               TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM
              MOVE WRK-49353           TO WRK-O-ITEMLS-ATTR
                                          (1)
              GO TO 4100-99-FIM
           END-IF.

           IF  WRK-QTD-SEL             GREATER 0
           AND WRK-QTD-DEL             GREATER 0
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'SELECIONAR APENAS UMA ACAO PARA REFBACEN'
                                       TO WRK-AUX-MENSA                                       
              PERFORM 4400-MOVER-MENSAGEM
              MOVE WRK-49353           TO WRK-O-ITEMLS-ATTR
                                          (WRK-IND-SEL)
              GO TO 4100-99-FIM
           END-IF.

           IF WRK-QTD-SEL              GREATER 1
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'SELECIONAR APENAS UMA LINHA PARA CONSULTA'
                                       TO WRK-AUX-MENSA                                       
              PERFORM 4400-MOVER-MENSAGEM
              MOVE WRK-49353           TO WRK-O-ITEMLS-ATTR
                                          (WRK-IND-SEL)
              GO TO 4100-99-FIM
           END-IF.

           IF WRK-QTD-DEL              GREATER 1
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'SELECIONAR APENAS UMA LINHA PARA EXCLUSAO'
                                       TO WRK-AUX-MENSA                                       
              PERFORM 4400-MOVER-MENSAGEM
              MOVE WRK-49353           TO WRK-O-ITEMLS-ATTR
                                          (WRK-IND-SEL)
              GO TO 4100-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4130-CONSISTIR-INCL-CONTR       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-INDINCL          TO WRK-INDINCL

           IF WRK-I-CODBAS-N(WRK-INDINCL)
                                       NOT NUMERIC
           OR WRK-I-CODBAS-N(WRK-INDINCL)
                                       EQUAL ZEROS
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'OBRIGATORIO INFORMAR CODIGO DE BASE LEGAL'
                                       TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM

              MOVE WRK-49353           TO WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)
              GO                       TO 4130-99-FIM
           END-IF.

           INITIALIZE RURCB00A

           MOVE WRK-I-CODBAS-N(WRK-INDINCL)
                                       TO NBASE-LEGAL-RENEG
                                       OF RURCB00A

           PERFORM 4131-VALIDAR-COD-BASE

           IF WRK-INCONSIS             EQUAL 'S'
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'CODIGO BASE LEGAL NAO ENCONTRADO NO RURC'
                                       TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM

              MOVE WRK-49353           TO WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)
              GO                       TO 4130-99-FIM
           END-IF.

           MOVE 4                      TO WRK-I-FASE.

      *----------------------------------------------------------------*
       4130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4131-VALIDAR-COD-BASE           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              SELECT RBASE_LEGAL_RENEG
                INTO :RURCB00A.RBASE-LEGAL-RENEG
                FROM DB2PRD.TBASE_LEGAL_RENEG
               WHERE NBASE_LEGAL_RENEG= :RURCB00A.NBASE-LEGAL-RENEG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RURCB00A'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '4131'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'S'                 TO WRK-INCONSIS
           END-IF.

      *----------------------------------------------------------------*
       4131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4140-CONSISTIR-INCL-SCONTR      SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           INSPECT WRK-I-SLDPRI(WRK-INDINCL)
                                       REPLACING LEADING  ' ' BY '0'

           INSPECT WRK-I-SLDREN(WRK-INDINCL)
                                       REPLACING LEADING  ' ' BY '0'

           INSPECT WRK-I-SLDENC(WRK-INDINCL)
                                       REPLACING LEADING  ' ' BY '0'

           COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                      (WRK-I-SLDPRI-N(WRK-INDINCL))

            MOVE WRK-193              TO WRK-O-SLDPRI-ATTR
                                         (WRK-INDINCL)
                                         WRK-O-SLDREN-ATTR
                                         (WRK-INDINCL)
                                         WRK-O-SLDENC-ATTR
                                         (WRK-INDINCL)
                                         WRK-O-CODBAS-ATTR
                                         (WRK-INDINCL)

           IF WRK-9-15V2-COMP         NOT NUMERIC
           OR WRK-9-15V2-COMP         EQUAL ZEROS
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'OBRIGATORIO INFORMAR SALDO PRINCIPAL'
                                       TO WRK-AUX-MENSA

              PERFORM 4400-MOVER-MENSAGEM

              MOVE WRK-49353           TO WRK-O-SLDPRI-ATTR
                                          (WRK-INDINCL)

              GO TO 4140-99-FIM
           END-IF.


           COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                      (WRK-I-SLDENC-N(WRK-INDINCL))

           IF WRK-9-15V2-COMP         NOT NUMERIC
           OR WRK-9-15V2-COMP         EQUAL ZEROS
              MOVE 'S'                TO WRK-INCONSIS
              MOVE 'OBRIGATORIO INFORMAR SALDO PRINCIPAL'
                                      TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM

              MOVE WRK-49353          TO WRK-O-SLDENC-ATTR
                                         (WRK-INDINCL)

              GO TO 4140-99-FIM
           END-IF.

           IF WRK-I-CODBAS-N(WRK-INDINCL)
                                       NOT NUMERIC
           OR WRK-I-CODBAS-N(WRK-INDINCL)
                                       EQUAL ZEROS
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'OBRIGATORIO INFORMAR CODIGO DE BASE LEGAL'
                                       TO WRK-AUX-MENSA

              PERFORM 4400-MOVER-MENSAGEM

              MOVE WRK-49353           TO WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)

              GO TO 4140-99-FIM
           END-IF.

           INITIALIZE RURCB00A
           MOVE WRK-I-CODBAS-N(WRK-INDINCL)
                                       TO NBASE-LEGAL-RENEG
                                       OF RURCB00A

           PERFORM 4131-VALIDAR-COD-BASE

           IF WRK-INCONSIS             EQUAL 'S'
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE 'CODIGO BASE LEGAL NAO ENCONTRADO NO RURC'
                                       TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM

              MOVE WRK-49353           TO WRK-O-CODBAS-ATTR
                                          (WRK-INDINCL)
              GO                       TO 4140-99-FIM
           END-IF.

           COMPUTE WRK-9-15V2-COMP = FUNCTION NUMVAL-C
                                      (WRK-I-SLDREN-N(WRK-INDINCL))

           IF WRK-9-15V2-COMP         NOT NUMERIC
           OR WRK-9-15V2-COMP         EQUAL ZEROS
              MOVE 'S'                TO WRK-INCONSIS
              MOVE 'OBRIGATORIO INFORMAR SALDO RENEGOCIACAO'
                                      TO WRK-AUX-MENSA

              PERFORM 4400-MOVER-MENSAGEM

              MOVE WRK-49353           TO WRK-O-SLDREN-ATTR
                                          (WRK-INDINCL)

              GO TO 4140-99-FIM
           END-IF.

           MOVE 5                     TO WRK-I-FASE.

      *----------------------------------------------------------------*
       4140-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4200-TOTALIZAR-PARCELAS         SECTION.
      *----------------------------------------------------------------*
           INITIALIZE WRK-AREA-7602.
      
           MOVE WRK-I-INDINCL          TO WRK-INDINCL
           MOVE WRK-O-CHVOPE-N(WRK-INDINCL)
                                       TO WRK-W602-E-CCONTR
      
           CALL WRK-RUCA7602           USING WRK-AREA-7602.
      
           IF WRK-W602-RET-CODE       NOT EQUAL ZEROS
              MOVE 'S'                 TO WRK-INCONSIS
              MOVE WRK-W602-RET-MENSAGEM
                                       TO WRK-AUX-MENSA
              PERFORM 4400-MOVER-MENSAGEM
              MOVE WRK-49353           TO WRK-O-REFBACEN-ATTR
                                          (WRK-INDINCL)
              GO TO 4200-99-FIM
           END-IF.
      
           MOVE WRK-W602-S-SLD-DEVEDOR TO WRK-I-SLDPRI-N
                                          (WRK-INDINCL).
      
           MOVE WRK-W602-S-SLD-ENCARGOS TO WRK-I-SLDENC-N
                                          (WRK-INDINCL).
      
      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4300-PASSAR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE SPACES                 TO WRK-TELA
                                          WRK-O-MENSA
                                          LK8-CODIGO-PROXPGM
                                          LK8-AREA-NAVEGA

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE LENGTH OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE 'RUEC7951'             TO WRK-COMU-TRANSACAO.
           MOVE '.'                    TO WRK-COMU-PFK.
           MOVE WRK-I-SENHAS           TO WRK-COMU-SENHAS.
           MOVE ZEROS                  TO WRK-COMU-OPCAO.
           MOVE WRK-I-CTERM            TO WRK-COMU-TERM.
           MOVE SPACES                 TO WRK-COMU-STATUS
                                          WRK-COMU-MENSAGEM.
           MOVE WRK-VALOR-TOTAL-AUX    TO WRK-COMU-VALOR-TOT
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

           IF WNAV-FUNCAO              OF NAVEGACAO-RURAL
                                       NOT EQUAL 'A'
              MOVE 'RUEC7030'          TO WRK-TELA
           END-IF.

           IF WRK-TELA                 NOT EQUAL SPACES
              GO TO 4300-99-FIM
           END-IF.

           PERFORM 4310-ACESSAR-RUEC8500

           IF LK8-CODIGO-PROXPGM       EQUAL SPACES OR 'RUEC8600'
              MOVE 'RUEC8030'          TO WRK-TELA
           ELSE
              MOVE LK8-CODIGO-PROXPGM  TO WRK-TELA
           END-IF.

           IF WNAV-FUNCAO              OF NAVEGACAO-RURAL
                                       NOT EQUAL 'A'
              MOVE 'RUEC7951'          TO CPROG OF RUECV007
              PERFORM 1221-UPDATE-RUECV007
           END-IF.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4310-ACESSAR-RUEC8500           SECTION.
      *----------------------------------------------------------------*

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO LK8-CODIGO-AGENCIA.
           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO LK8-CODIGO-CEDULA.
           MOVE WRK-I-CTERM            TO LK8-CODIGO-TERMINAL.

           MOVE 07                     TO LK8-NUMORD-CHAMADOR.
           MOVE 'RUEC8500'             TO WRK-MODULO.
           CALL WRK-MODULO             USING LNK-IO-PCB
                                             LNK-ALT-PCB
                                             LK8-AREA-NAVEGA.

           IF LK8-STATUS-RETORNO       EQUAL SPACES
              GO TO 4310-99-FIM
           END-IF.

           EVALUATE LK8-STATUS-RETORNO
           WHEN 'ER'
               MOVE LK8-MENSAGEM       TO WRK-O-MENSA
           WHEN 'BL'
               MOVE LK8-MENSAGEM       TO WRK-O-MENSA
           WHEN 'DB'
               MOVE LK8-AREA-ENTRADA   TO ERR-TEXTO(01:24)
               MOVE LK8-STATUS-RETORNO TO ERR-TEXTO(25:02)
               MOVE LK8-CODIGO-PROXPGM TO ERR-TEXTO(27:08)
               MOVE LK8-MENSAGEM       TO ERR-TEXTO(35:31)
               MOVE 'RUEC8500'         TO ERR-MODULO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       4310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4400-MOVER-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           IF WRK-O-MENSA              EQUAL SPACES
              MOVE WRK-AUX-MENSA       TO WRK-O-MENSA
           END-IF.

      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5100-TRATAR-PF11                SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-VALOR-TOTAL = FUNCTION NUMVAL-C(WRK-I-SLDTOT-N)
           MOVE WRK-VALOR-TOTAL        TO WRK-VALOR-TOTAL-AUX
                                          WRK-O-SLDTOT-N

           PERFORM 4300-PASSAR-CONTROLE.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       7000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT
                                       TO WRK-660-LL-OUTPUT
                                          WRK-O-LL
           MOVE LENGTH                 OF WRK-660-RUECT797
                                       TO WRK-660-LL

           CALL 'BRAD0660'             USING WRK-OUTPUT
                                             WRK-660-RUECT797.

           IF RETURN-CODE              NOT EQUAL ZEROS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE RETURN-CODE         TO WRK-RETURN-CODE
              MOVE 70                  TO WRK-LOCAL-ERRO
              MOVE WRK-ERRO-BRAD0660   TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT             TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO               SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO               NOT EQUAL SPACES AND LOW-VALUES
              MOVE 'RUEC7951'          TO ERR-PGM
           ELSE
              MOVE 'SENHAS02'          TO ERR-PGM
              MOVE 'RUEC7951'          TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER-X         TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.

           IF ERR-TIPO-ACESSO          EQUAL 'DB2'
              CALL 'BRAD7100'          USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             SQLCA
           ELSE
              CALL 'BRAD7100'          USING LNK-IO-PCB
                                             ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
