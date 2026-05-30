       IDENTIFICATION DIVISION.
      **************************
       PROGRAM-ID.    BVVE1665.
       DATE-COMPILED.
      *REMARKS.
      *   *******************************************************
      *   *                      OBJETIVO                       *
      *   *      MONTAR TELA COM NUMERO DE CARTOES DISPO-       *
      *   *      NIVEIS PARA O VENDEDOR, A PARTIR DA AGEN-      *
      *   *      CIA, CONTA E DATA TECLADA OU SOMENTE AGEN-     *
      *   *      CIA E CONTA.                                   *
      ***********************************************************
*/////****************************************************************
*/////*    U  L  T  I  M  A    A  L  T  E  R  A  C  A  O   :         *
*/////****************************************************************
*/////*                                                              *
      * PROGRAMADOR...: LIBERTO                - CPM                 *
      * SUPERVISOR....: JOAO CABRAL            - CPM                 *
      * ANALISTA......: GERTRUD                - CPM - GRUPO 90      *
      * DATA..........: NOVEMBRO/99                                  *
*/////*                                                              *
      * OBJETIVO......: CONVERSAO DE BASE DE DADOS DL/I PARA DB2     *
*/////****************************************************************
      /
       ENVIRONMENT DIVISION.
      **********************


       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      /
       DATA DIVISION.
      ***************


       WORKING-STORAGE SECTION.
      *-----------------------*


ROMULO*-------------------------------------------------------*
ROMULO*        A R E A    D A   P O O L 7 6 0 0
ROMULO*-------------------------------------------------------*
-INC I#BVGEDH
ROMULO
ROMULO 01  WRK-DATA-POOL7600        PIC 9(08).
ROMULO 01  FILLER REDEFINES WRK-DATA-POOL7600.
ROMULO     03  WRK-AAAA-POOL7600    PIC 9(04).
ROMULO     03  WRK-MM-POOL7600      PIC 9(02).
ROMULO     03  WRK-DD-POOL7600      PIC 9(02).
ROMULO
ROMULO 01  WRK-DATA-POOL7600-R.
ROMULO     03  WRK-DD-POOL7600-E    PIC 9(02).
ROMULO     03  FILLER               PIC X(01) VALUE '/'.
ROMULO     03  WRK-MM-POOL7600-E    PIC 9(02).
ROMULO     03  FILLER               PIC X(01) VALUE '/'.
ROMULO     03  WRK-AAAA-POOL7600-E  PIC 9(04).
ROMULO 01  WRK-DATA-POOL7600-E REDEFINES WRK-DATA-POOL7600-R
                                    PIC X(10).
ROMULO
ROMULO*-------------------------------------------------------*
ROMULO* F I M   D A   A R E A   D A   P O O L 7 6 0 0
ROMULO*-------------------------------------------------------*
ROMULO

       01  WRK-FUNCOES.
           03  GU                      PIC  X(004)  VALUE 'GU  '.
           03  CHNG                    PIC  X(004)  VALUE 'CHNG'.
           03  ISRT                    PIC  X(004)  VALUE 'ISRT'.
           03  FUNCAO                  PIC  X(004)  VALUE SPACES.

       01  POS-CURSOR.
           03  POS-LINHA               PIC  9(002).
           03  POS-COLUNA              PIC  9(002).

       01  ATUAL-OPCAO             PIC  9(002).
       01  NOVA-OPCAO              PIC  9(002).
       01  USUARIO                 PIC  9(007).
       01  DPTO.
           03  FILLER                  PIC  X(002).
           03  DEPTO                   PIC  9(005).
       01  MODNAME                 PIC  X(008)  VALUE   SPACES.
       01  BVVET665                PIC  X(008)  VALUE   'BVVET665'.
       01  BVVE1660                PIC  X(008)  VALUE   'BVVE1660'.
       01  WRK-BVVE1665                PIC  X(008)  VALUE   'BVVE1665'.
       01  BVVE1670                PIC  X(008)  VALUE   'BVVE1670'.
       01  BVVE1681                PIC  X(008)  VALUE   'BVVE1681'.
       01  IND                     PIC  9(005)  COMP-3  VALUE ZEROS.
       01  NOT-FOUND               PIC S9(009)  COMP-4  VALUE  +100.
       01  WRK-IN-DATA.
           03  WRK-DIA                PIC 9(02)         VALUE ZEROS.
           03  FILLER                 PIC X(01).
           03  WRK-MES                PIC 9(02)         VALUE ZEROS.
           03  FILLER                 PIC X(01).
ROM        03  WRK-SEC                PIC 9(02)         VALUE ZEROS.
           03  WRK-ANO                PIC 9(02)         VALUE ZEROS.
       01  WRK-NUM-CARTAO.
           03  WRK-NUM-CARTAO-PREFX   PIC 9(04)         VALUE ZEROS.
           03  WRK-NUM-CARTAO-CBCO    PIC 9(03)         VALUE ZEROS.
           03  WRK-NUM-CARTAO-CONVN   PIC 9(06)         VALUE ZEROS.
       01  WRK-NUM-CARTAO-R   REDEFINES   WRK-NUM-CARTAO   PIC 9(13).

      /---------------------------------------------------------------*
      *   DEFINICAO DA TABELA P/ DESCOBRIR POSICIONAMENTO DO CURSOR   *
      *---------------------------------------------------------------*

       01  TABELA-POSICAO.
           03  TABELA-CURSOR.
               05  FILLER           PIC X(07) VALUE '0704 01'.
               05  FILLER           PIC X(07) VALUE '0804 02'.
               05  FILLER           PIC X(07) VALUE '0904 03'.
               05  FILLER           PIC X(07) VALUE '1004 04'.
               05  FILLER           PIC X(07) VALUE '1104 05'.
               05  FILLER           PIC X(07) VALUE '1204 06'.
               05  FILLER           PIC X(07) VALUE '1304 07'.
               05  FILLER           PIC X(07) VALUE '1404 08'.
               05  FILLER           PIC X(07) VALUE '1504 09'.
               05  FILLER           PIC X(07) VALUE '1604 10'.
               05  FILLER           PIC X(07) VALUE '1704 11'.
               05  FILLER           PIC X(07) VALUE '1804 12'.
               05  FILLER           PIC X(07) VALUE '1904 13'.
           03  TAB-CURSOR    REDEFINES        TABELA-CURSOR
                             OCCURS 13 TIMES  INDEXED BY   TAB.
               05  LIN-COL          PIC 9(04).
               05  FILLER           PIC X(01).
               05  PRO-IND          PIC 9(02).


      /---------------------------------------------------------------*
      *        DEFINICAO DA AREA P/ RECEBER CPOS DO DB2               *
      *---------------------------------------------------------------*

       01  AREAS-DB2.
           03  SEL-AGENCIA          PIC S9(05) COMP-3   VALUE ZEROS.
           03  SEL-CONTA            PIC S9(07) COMP-3   VALUE ZEROS.
           03  SEL-NUM-CARTAO       PIC S9(13) COMP-3   VALUE ZEROS.
           03  SEL-DT-FECH          PIC X(10)           VALUE SPACES.

      /---------------------------------------------------------------*
      *            DEFINICAO DA AREA P/ RECEBER MENSAGENS             *
      *---------------------------------------------------------------*
       01  MENSAGEM.
           03  FILLER               PIC 9(004)  COMP VALUE ZEROS.
           03  FILLER               PIC 9(004)  COMP VALUE ZEROS.
           03  FILLER               PIC X(015).
           03  IN-POOLMIL-AREA      PIC X(037).
           03  IN-POOLMIL-OPC       PIC 9(002).
           03  FILLER               PIC X(1870).

      *---------------------------------------------------------------*
      *           RECEBE MENSAGEM DE INPUT DA TELA BVVET665           *
      *           E DOS PROGRAMAS      BVVE1660  E BVVE1670           *
      *---------------------------------------------------------------*

       01  FILLER   REDEFINES  MENSAGEM.
           03  IN-LL                    PIC 9(4)   COMP.
           03  IN-ZZ                    PIC 9(4)   COMP.
           03  IN-TRANSA                PIC X(14).
           03  IN-PFK                   PIC X(01).
           03  FILLER                   PIC X(39).
           03  IN-CPOS-RECEBIDOS.
               05  IN-SEG-VIA-INV       PIC X(01).
               05  IN-TIPO-PESQ-INV     PIC 9(02).
               05  IN-AGENCIA           PIC S9(04).
               05  IN-CONTA             PIC S9(07).
               05  IN-CARTAO            PIC S9(13).
               05  IN-DT-RECEBIDA.
                   07  IN-DD            PIC 9(02).
                   07  IN-MM            PIC 9(02).
ROMULO             07  IN-SS            PIC 9(02).
                   07  IN-AA            PIC 9(02).
           03  IN-CURSOR.
               05  IN-LINHA             PIC 9(04) COMP.
               05  IN-COLUNA            PIC 9(04) COMP.
           03  IN-MARCA-FIM             PIC  X(01).
           03  IN-PAGINA                PIC S9(03).
           03  IN-OCORRENCIAS   OCCURS   13  TIMES.
               05  IN-DT-FECH.
                   07  IN-DIA           PIC 9(02).
                   07  FILLER           PIC X(01).
                   07  IN-MES           PIC 9(02).
                   07  FILLER           PIC X(01).
ROMULO             07  IN-SEC           PIC 9(02).
                   07  IN-ANO           PIC 9(02).
               05  IN-NUM-CARTAO        PIC S9(13).
               05  IN-CARTAO-R  REDEFINES  IN-NUM-CARTAO     PIC X(13).

      /---------------------------------------------------------------*
      *     PASSA CONTROLE PARA OS PROGRAMAS  BVVE1670 E BVVE1660     *
      *---------------------------------------------------------------*

       01  PS-MENSAGEM.
           03  PS-LL                    PIC 9(4)     COMP VALUE ZEROS.
           03  PS-ZZ                    PIC 9(4)     COMP VALUE ZEROS.
           03  PS-TRANSA                PIC X(14)    VALUE 'BVVE1665'.
           03  FILLER                   PIC X(01)    VALUE '.'.
           03  PS-POOLMIL-AREA          PIC X(37).
           03  PS-POOLMIL-OPC           PIC 9(02).
           03  PS-CPOS-RECEBIDOS.
               05  PS-SEG-VIA-INV       PIC X(01).
               05  PS-TIPO-PESQ-INV     PIC 9(02).
               05  PS-AGENCIA           PIC 9(04).
               05  PS-CONTA             PIC 9(07).
               05  PS-CARTAO-REST       PIC 9(13).
               05  PS-DT-FECH.
                   07  PS-DIA           PIC 9(02).
                   07  PS-MES           PIC 9(02).
ROMULO             07  PS-SEC           PIC 9(02).
                   07  PS-ANO           PIC 9(02).
               05  PS-MANEIRA-PESQ      PIC 9(01).
           03  FILLER               PIC X(02)    VALUE SPACES.

      /---------------------------------------------------------------*
      *          ENVIA  MENSAGEM DE OUTPUT P/ TELA BVVET665           *
      *---------------------------------------------------------------*

       01  OU-MENSAGEM.
           03  OU-LL                PIC 9(04)    COMP VALUE ZEROS.
           03  OU-ZZ                PIC 9(04)    COMP VALUE ZEROS.
           03  OU-POOLMIL-AREA      PIC X(37).
ROMULO     03  OU-DATASYS           PIC X(10).
           03  OU-MSGPGM            PIC X(79)         VALUE SPACES.
           03  OU-SEG-VIA-INV       PIC X(01)         VALUE SPACE.
           03  OU-TIPO-PESQ-INV     PIC 9(02)         VALUE ZEROS.
           03  OU-AGENCIA           PIC 9(04)         VALUE ZEROS.
           03  OU-CONTA             PIC 9(07)         VALUE ZEROS.
           03  OU-CARTAO            PIC 9(13)         VALUE ZEROS.
           03  OU-DT-RECEBIDA.
               05  OU-DD            PIC 9(02)         VALUE ZEROS.
               05  OU-MM            PIC 9(02)         VALUE ZEROS.
ROMULO         05  OU-SS            PIC 9(02)         VALUE ZEROS.
               05  OU-AA            PIC 9(02)         VALUE ZEROS.
           03  OU-MARCA-FIM         PIC  X(01)        VALUE ZEROS.
           03  OU-PAGINA            PIC  9(03)        VALUE ZEROS.
           03  OU-OCORRENCIAS   OCCURS   13  TIMES.
               05  OU-ATR-SELCT         PIC S9(04) COMP.
               05  FILLER               PIC  X(01).
               05  OU-DT-FECH.
                   07  OU-DIA               PIC 9(02).
                   07  OU-BARRA1            PIC X(01).
                   07  OU-MES               PIC 9(02).
                   07  OU-BARRA2            PIC X(01).
ROMULO             07  OU-SEC               PIC 9(02).
                   07  OU-ANO               PIC 9(02).
               05  OU-NUM-CARTAO        PIC  9(13).
               05  OU-NOME              PIC  X(36).

      /
       01  ERR-MSG.
           05  FILLER                PIC  X(49) VALUE
               '*** PROBLEMAS COM A   POOL0660     RETURN CODE = '.
           05  ERR-RETURN-CODE       PIC  9(02).
           05  FILLER                PIC  X(13) VALUE '     LOCAL = '.
           05  ERR-LOCAL-MSG         PIC  9(03).
           05  FILLER                PIC  X(04) VALUE ' ***'.

      /
-INC POL7100C


      /---------------------------------------------------------------*
      *  AREA DA POOL0660 COM MASCARA DA TELA BVVET665 - SEGMENTO 00  *
      *---------------------------------------------------------------*
       01  660-BVVET665-SEG00.
ROM        03  660-LL-AREA-T012        PIC  9(04)  COMP  VALUE 0182.
ROM        03  660-LL-MENSAGEM-T012    PIC  9(04)  COMP  VALUE 0975.
           03  660-CPOMIL-T012         PIC  9(04)  COMP  VALUE 0037.
ROM        03  660-DATASYS-T012        PIC  9(04)  COMP  VALUE 0010.
           03  660-SYSM-T012           PIC  9(04)  COMP  VALUE 0079.
           03  660-SEGVIA-T012         PIC  9(04)  COMP  VALUE 0001.
           03  660-TPOPESQ-T012        PIC  9(04)  COMP  VALUE 0002.
           03  660-AGENCIA-T012        PIC  9(04)  COMP  VALUE 0004.
           03  660-CONTA-T012          PIC  9(04)  COMP  VALUE 0007.
           03  660-CARTAO-T012         PIC  9(04)  COMP  VALUE 0013.
ROM        03  660-DATA-T012           PIC  9(04)  COMP  VALUE 0008.
           03  660-MARFIM-T012         PIC  9(04)  COMP  VALUE 0001.
           03  660-PAGINA-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.
           03  660-SELCT-T012          PIC  9(04)  COMP  VALUE 2003.
           03  660-DATACRED-T012       PIC  9(04)  COMP  VALUE 0010.
           03  660-CARTP1-T012         PIC  9(04)  COMP  VALUE 0004.
           03  660-CARTP2-T012         PIC  9(04)  COMP  VALUE 0003.
           03  660-CARTP3-T012         PIC  9(04)  COMP  VALUE 0006.
           03  660-NOME-T012           PIC  9(04)  COMP  VALUE 0036.

      /---------------------------------------------------------------*
      *        INCLUDE PARA AS DEFINICOES DA TABELA DB2               *
      *---------------------------------------------------------------*
           EXEC SQL
                INCLUDE  KELOB004
           END-EXEC.

           EXEC SQL
                INCLUDE  BACCV014
           END-EXEC.

      *---------------------------------------------------------------*
      *                SQLCA    DOS    ACESSOS    DB2                 *
      *---------------------------------------------------------------*
           EXEC SQL
                INCLUDE  SQLCA
           END-EXEC.

      /---------------------------------------------------------------*
      *      DECLARACAO DO CURSOR PARA SELECT  (AGENCIA E CONTA)      *
      *---------------------------------------------------------------*
           EXEC SQL
                DECLARE  C1                   CURSOR   FOR
                   SELECT  DISTINCT   FATON_DT_FECHA,
                                      FATON_NUM_CARTAO,
                                      FATON_AGENCIA,
                                      FATON_CONTA
                   FROM     DB2PRD.KELO_FAT_ONLINE
                   WHERE    FATON_AGENCIA     =  :SEL-AGENCIA     AND
                            FATON_CONTA       =  :SEL-CONTA
                   ORDER BY FATON_DT_FECHA,
                            FATON_NUM_CARTAO
           END-EXEC.



      *---------------------------------------------------------------*
      * DECLARACAO DO CURSOR PARA SELECT  (AGENCIA, CONTA E DATA CRED)*
      *---------------------------------------------------------------*
           EXEC SQL
                DECLARE  C2                   CURSOR   FOR
                   SELECT  DISTINCT   FATON_DT_FECHA,
                                      FATON_NUM_CARTAO,
                                      FATON_AGENCIA,
                                      FATON_CONTA
                   FROM     DB2PRD.KELO_FAT_ONLINE
                   WHERE    FATON_AGENCIA     =  :SEL-AGENCIA     AND
                            FATON_CONTA       =  :SEL-CONTA       AND
                            FATON_DT_FECHA    =  :SEL-DT-FECH
                   ORDER BY FATON_DT_FECHA,
                            FATON_NUM_CARTAO
           END-EXEC.


      /---------------------------------------------------------------*
      *      DECLARACAO DO CURSOR PARA SELECT  (AGENCIA E CONTA       *
      *                                   COM CARTAO ESPECIFICO)      *
      *---------------------------------------------------------------*
           EXEC SQL
                DECLARE  C3                   CURSOR   FOR
                   SELECT  DISTINCT   FATON_DT_FECHA,
                                      FATON_NUM_CARTAO,
                                      FATON_AGENCIA,
                                      FATON_CONTA
                   FROM     DB2PRD.KELO_FAT_ONLINE
                   WHERE    FATON_NUM_CARTAO  =  :SEL-NUM-CARTAO  AND
                            FATON_AGENCIA     =  :SEL-AGENCIA     AND
                            FATON_CONTA       =  :SEL-CONTA
                   ORDER BY FATON_DT_FECHA,
                            FATON_NUM_CARTAO
           END-EXEC.

      /
       LINKAGE SECTION.
      *---------------*

       01  IO-PCB.
           03  IO-LTERM          PIC  X(008).
           03  FILLER            PIC  X(002).
           03  IO-STATUS         PIC  X(002).
           03  FILLER            PIC  X(012).
           03  IO-MODNAME        PIC  X(008).

       01  ALT-PCB.
           03  ALT-LTERM         PIC  X(008).
           03  FILLER            PIC  X(002).
           03  ALT-STATUS        PIC  X(002).

      /
       PROCEDURE DIVISION.
      ********************


           ENTRY   'DLITCBL'   USING   IO-PCB   ALT-PCB.

       010-LER-MSG.
      *-----------*

           MOVE     'SENHAS02'    TO     ERR-PGM.
           MOVE     GU            TO     FUNCAO.

       020-PROCESSA-FUNCAO.
      *-------------------*

           CALL 'POOL5000'  USING  FUNCAO    IO-PCB       ALT-PCB
                                   MENSAGEM  MODNAME      NOVA-OPCAO
                                   USUARIO   ATUAL-OPCAO  DPTO.


ROMULO     IF   RETURN-CODE   EQUAL   4   OR  12
ROMULO          GOBACK.


           IF  IN-PFK   EQUAL   'C'
               MOVE     58                     TO  PS-LL
               MOVE    WRK-BVVE1665               TO  PS-TRANSA
               MOVE     IN-POOLMIL-AREA        TO  PS-POOLMIL-AREA
               MOVE     IN-POOLMIL-OPC         TO  PS-POOLMIL-OPC
               MOVE     BVVE1660               TO  MODNAME
               MOVE     CHNG                   TO  FUNCAO
               MOVE     PS-MENSAGEM            TO  MENSAGEM
               GO                        TO    020-PROCESSA-FUNCAO.


           MOVE  IN-AGENCIA        TO  SEL-AGENCIA.
           MOVE  IN-CONTA          TO  SEL-CONTA.

           IF  IN-TRANSA  EQUAL  WRK-BVVE1665
               GO   TO   180-VERIFICA-SE-RESTART.

      /
       030-INICIA.
      *----------*

           MOVE  0                 TO  IND.
           MOVE  1                 TO  OU-PAGINA.

       040-PRIMEIRA-FASE.
      *-----------------*

           IF  IN-TIPO-PESQ-INV   EQUAL   03
               GO    TO     120-PESQ-AGE-CTA-DT.

           IF  IN-CARTAO   NOT EQUAL   ZEROS
               GO    TO     080-PESQ-AGE-CTA-ESP.

           EXEC SQL
                OPEN  C1
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  01    TO      ERR-LOCAL
               MOVE  'OPEN'        TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

       050-FETCH-AGE-CTA.
      *-----------------*

           EXEC SQL
                FETCH   C1              INTO     :SEL-DT-FECH,
                                                 :SEL-NUM-CARTAO,
                                                 :SEL-AGENCIA,
                                                 :SEL-CONTA
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   NOT-FOUND
               GO  TO    055-VE-SQL-FETCH.


           EXEC SQL
                CLOSE C1
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  02    TO      ERR-LOCAL
               MOVE  'CLOSE'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

ROMULO     COMPUTE   OU-LL =   169  +   ( 60 * IND ).
           MOVE  '** POSICIONE O CURSOR E TECLE ENTER OU UMA DAS PF''S -
      -    ' FIM DE AMOSTRAGEM **'     TO   OU-MSGPGM.
           MOVE  '*'              TO   OU-MARCA-FIM.
           GO    TO    160-INSERE-BVVET665.


       055-VE-SQL-FETCH.
      *----------------*

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  03    TO      ERR-LOCAL
               MOVE  'FETCH'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

           IF  IND   GREATER   12
               GO    TO     070-LOTOU-A-TELA.


       060-COMECA-A-LOTAR.
      *------------------*

           ADD    1               TO  IND.

           PERFORM   170-PESQUISA-NOME.

           MOVE   212             TO  OU-ATR-SELCT(IND).
           MOVE   SEL-NUM-CARTAO  TO  OU-NUM-CARTAO(IND).
           MOVE   '/'             TO  OU-BARRA1(IND)  OU-BARRA2(IND).
           MOVE   SEL-DT-FECH     TO  WRK-IN-DATA.
           MOVE   WRK-DIA         TO  OU-DIA(IND).
           MOVE   WRK-MES         TO  OU-MES(IND).
ROMULO     MOVE   WRK-SEC         TO  OU-SEC(IND).
           MOVE   WRK-ANO         TO  OU-ANO(IND).

           GO    TO     050-FETCH-AGE-CTA.

       070-LOTOU-A-TELA.
      *----------------*

           EXEC SQL
                CLOSE C1
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  04    TO      ERR-LOCAL
               MOVE  'CLOSE'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

ROMULO     MOVE   975              TO  OU-LL.
           MOVE   '* POSICIONE O CURSOR E TECLE ENTER OU UMA DAS PF''S -
      -    ' AMOSTRAGEM CONTINUA *' TO   OU-MSGPGM.
           MOVE   SPACES        TO  OU-MARCA-FIM.
           GO     TO      160-INSERE-BVVET665.

      /
       080-PESQ-AGE-CTA-ESP.
      *--------------------*

           MOVE  IN-CARTAO         TO  SEL-NUM-CARTAO.

           EXEC SQL
                OPEN  C3
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  05    TO      ERR-LOCAL
               MOVE  'OPEN'        TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

       090-FETCH-AGE-CTA-ESP.
      *---------------------*

           EXEC SQL
                FETCH   C3              INTO     :SEL-DT-FECH,
                                                 :SEL-NUM-CARTAO,
                                                 :SEL-AGENCIA,
                                                 :SEL-CONTA
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   NOT-FOUND
               GO  TO    095-VE-SQL-FETCH.


           EXEC SQL
                CLOSE C3
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  06    TO      ERR-LOCAL
               MOVE  'CLOSE'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

ROMULO     COMPUTE   OU-LL =   169  +   ( 60 * IND )
           MOVE  '** POSICIONE O CURSOR E TECLE ENTER OU UMA DAS PF''S -
      -    ' FIM DE AMOSTRAGEM **'     TO   OU-MSGPGM.
           MOVE  '*'              TO   OU-MARCA-FIM.
           GO        TO    160-INSERE-BVVET665.


       095-VE-SQL-FETCH.
      *----------------*

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  07    TO      ERR-LOCAL
               MOVE  'FETCH'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

           IF  IND   GREATER   12
               GO    TO     110-LOTOU-A-TELA-ESP.

       100-COMECA-A-LOTAR-ESP.
      *----------------------*

           ADD    1               TO  IND.

           PERFORM   170-PESQUISA-NOME.

           MOVE   212             TO  OU-ATR-SELCT(IND).
           MOVE   SEL-NUM-CARTAO  TO  OU-NUM-CARTAO(IND).
           MOVE   '/'             TO  OU-BARRA1(IND)  OU-BARRA2(IND).
           MOVE   SEL-DT-FECH     TO  WRK-IN-DATA.
           MOVE   WRK-DIA         TO  OU-DIA(IND).
           MOVE   WRK-MES         TO  OU-MES(IND).
ROM        MOVE   WRK-SEC         TO  OU-SEC(IND).
           MOVE   WRK-ANO         TO  OU-ANO(IND).

           GO    TO     090-FETCH-AGE-CTA-ESP.

       110-LOTOU-A-TELA-ESP.
      *--------------------*

           EXEC SQL
                CLOSE C3
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  08    TO      ERR-LOCAL
               MOVE  'CLOSE'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

ROMULO     MOVE   975              TO  OU-LL.
           MOVE   '* POSICIONE O CURSOR E TECLE ENTER OU UMA DAS PF''S -
      -    ' AMOSTRAGEM CONTINUA *' TO   OU-MSGPGM.
           MOVE   SPACES       TO  OU-MARCA-FIM.
           GO     TO      160-INSERE-BVVET665.

      /
       120-PESQ-AGE-CTA-DT.
      *-------------------*


           STRING  IN-DD  '.'
                   IN-MM  '.'  IN-SS
                   IN-AA  DELIMITED BY SIZE
                   INTO SEL-DT-FECH.

           EXEC SQL
                OPEN  C2
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  09    TO      ERR-LOCAL
               MOVE  'OPEN'        TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

       130-FETCH-AGE-CTA-DT.
      *--------------------*

           EXEC SQL
                FETCH   C2              INTO     :SEL-DT-FECH,
                                                 :SEL-NUM-CARTAO,
                                                 :SEL-AGENCIA,
                                                 :SEL-CONTA
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   NOT-FOUND
               GO  TO    135-VE-SQL-FETCH.


           EXEC SQL
                CLOSE C2
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  10    TO      ERR-LOCAL
               MOVE  'CLOSE'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

ROMULO     COMPUTE   OU-LL =   169  +   ( 60 * IND ).
           MOVE  '** POSICIONE O CURSOR E TECLE ENTER OU UMA DAS PF''S -
      -    ' FIM DE AMOSTRAGEM **'     TO   OU-MSGPGM.
           MOVE '*'                TO  OU-MARCA-FIM.
           GO        TO    160-INSERE-BVVET665.

       135-VE-SQL-FETCH.
      *----------------*

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  11    TO      ERR-LOCAL
               MOVE  'FETCH'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

           IF  IND   GREATER   12
               GO    TO     150-ENCHEU-A-TELA.

       140-COMECA-A-ENCHER.
      *-------------------*

           ADD    1               TO  IND.

           PERFORM   170-PESQUISA-NOME.

           MOVE   212             TO  OU-ATR-SELCT(IND).
           MOVE   SEL-NUM-CARTAO  TO  OU-NUM-CARTAO(IND).
           MOVE   '/'             TO  OU-BARRA1(IND)  OU-BARRA2(IND).
           MOVE   SEL-DT-FECH     TO  WRK-IN-DATA.
           MOVE   WRK-DIA         TO  OU-DIA(IND).
           MOVE   WRK-MES         TO  OU-MES(IND).
ROM        MOVE   WRK-SEC         TO  OU-SEC(IND).
           MOVE   WRK-ANO         TO  OU-ANO(IND).

           GO    TO     130-FETCH-AGE-CTA-DT.

       150-ENCHEU-A-TELA.
      *-----------------*

           EXEC SQL
                CLOSE C2
           END-EXEC

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  12    TO      ERR-LOCAL
               MOVE  'CLOSE'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

ROMULO     MOVE   975              TO  OU-LL.
           MOVE   '* POSICIONE O CURSOR E TECLE ENTER OU UMA DAS PF''S -
      -    ' AMOSTRAGEM CONTINUA *'     TO   OU-MSGPGM.
           MOVE   SPACES            TO  OU-MARCA-FIM.
           GO     TO      160-INSERE-BVVET665.

      /
       160-INSERE-BVVET665.
      *-------------------*

           MOVE   IN-POOLMIL-AREA    TO  OU-POOLMIL-AREA.
           MOVE   IN-TIPO-PESQ-INV   TO  OU-TIPO-PESQ-INV.
           MOVE   IN-AGENCIA         TO  OU-AGENCIA.
           MOVE   IN-CONTA           TO  OU-CONTA.
           MOVE   IN-CARTAO          TO  OU-CARTAO.
           MOVE   IN-DT-RECEBIDA     TO  OU-DT-RECEBIDA.
           MOVE   IN-SEG-VIA-INV     TO  OU-SEG-VIA-INV.
           MOVE   BVVET665           TO  MODNAME.

ROMULO     CALL  'POOL7600'     USING    DATA-HORA.
ROMULO
ROMULO     MOVE  DT-AAAAMMDD          TO     WRK-DATA-POOL7600.
ROMULO     MOVE  WRK-AAAA-POOL7600    TO     WRK-AAAA-POOL7600-E.
ROMULO     MOVE  WRK-MM-POOL7600      TO     WRK-MM-POOL7600-E.
ROMULO     MOVE  WRK-DD-POOL7600      TO     WRK-DD-POOL7600-E.
ROMULO     MOVE  WRK-DATA-POOL7600-E  TO     OU-DATASYS.

           CALL 'POOL0660'   USING   OU-MENSAGEM, 660-BVVET665-SEG00.
           IF  RETURN-CODE  NOT EQUAL ZERO
               MOVE    'APL'        TO      ERR-TIPO-ACESSO
               MOVE    RETURN-CODE  TO      ERR-RETURN-CODE
               MOVE    17           TO      ERR-LOCAL-MSG
               MOVE    ERR-MSG      TO      ERR-TEXTO
               GO                   TO      999-CALL7100.

           MOVE  OU-MENSAGEM        TO      MENSAGEM.
           MOVE  ISRT               TO      FUNCAO.

           GO                   TO  020-PROCESSA-FUNCAO.

      /
       170-PESQUISA-NOME.
      *-----------------*

           MOVE  SEL-NUM-CARTAO        TO  WRK-NUM-CARTAO-R.
           MOVE  WRK-NUM-CARTAO-PREFX  TO  CPREFX-ESTBL-BACC.
           MOVE  WRK-NUM-CARTAO-CBCO   TO  CBCO-VISA.
           MOVE  WRK-NUM-CARTAO-CONVN  TO  CESTBL-CONVN-BACC.

           EXEC SQL
             SELECT
                   IFANTS
             INTO
                   :BACCV014.IFANTS
             FROM   DB2PRD.V01ESTBL_CONVN_BAC
             WHERE
                   CPREFX_ESTBL_BACC  = :BACCV014.CPREFX-ESTBL-BACC  AND
                   CBCO_VISA          = :BACCV014.CBCO-VISA          AND
                   CESTBL_CONVN_BACC  = :BACCV014.CESTBL-CONVN-BACC
           END-EXEC.
XXXX
           IF  SQLCODE  EQUAL  +000
               MOVE  IFANTS                  TO  OU-NOME(IND)
XXXX       ELSE
               IF  SQLCODE  EQUAL  +100
                   MOVE  SPACES              TO   OU-NOME(IND)
XXXX           ELSE
                   MOVE 'DB2'                TO   ERR-TIPO-ACESSO
                   MOVE 'V01ESTBL_CONVN_BAC' TO   ERR-DBD-TAB
                   MOVE 'SELECT  '           TO   ERR-FUN-COMANDO
                   MOVE SQLCODE              TO   ERR-SQL-CODE
                   MOVE 18                   TO   ERR-LOCAL
                   MOVE SPACES               TO   ERR-SEGM
XXXX               PERFORM       999-CALL7100.

      /
       180-VERIFICA-SE-RESTART.
      *-----------------------*



           IF  IN-PFK   EQUAL   '7'

               IF  IN-PAGINA   GREATER   1
                   COMPUTE   OU-PAGINA  =  IN-PAGINA - 1
                   COMPUTE   IND        =  13 * (IN-PAGINA - 2)

                   IF  IN-DT-RECEBIDA   EQUAL   ZEROS

                       IF  IN-CARTAO   EQUAL   ZEROS
                           GO   TO  210-DESPREZA-COM-C1-IND
                       ELSE
                           GO   TO  230-DESPREZA-COM-C3-IND

                   ELSE

                       GO   TO  250-DESPREZA-COM-C2-IND

               ELSE

                   GO   TO  200-MONTA-CARTAO-ATUAL.

      /


           IF  IN-PFK             EQUAL   '8'     AND
               IN-MARCA-FIM   NOT EQUAL   '*'

               COMPUTE   OU-PAGINA  =  IN-PAGINA + 1
               COMPUTE   IND        =  13 * IN-PAGINA

               IF  IN-DT-RECEBIDA   EQUAL   ZEROS

                   IF  IN-CARTAO   EQUAL   ZEROS
                       GO   TO  210-DESPREZA-COM-C1-IND

                   ELSE

                       GO   TO  230-DESPREZA-COM-C3-IND

               ELSE

                   GO   TO  250-DESPREZA-COM-C2-IND.

      /

           MOVE  IN-LINHA     TO  POS-LINHA.
           MOVE  IN-COLUNA    TO  POS-COLUNA.
           SET   TAB          TO  1.


           SEARCH  TAB-CURSOR
                   VARYING    TAB
                   AT END     GO  TO   200-MONTA-CARTAO-ATUAL
                   WHEN       LIN-COL(TAB)  =  POS-CURSOR
ROMULO                        NEXT SENTENCE.


           MOVE  PRO-IND(TAB)     TO  IND.


           IF  IN-CARTAO-R(IND)   NOT   NUMERIC
               GO  TO   200-MONTA-CARTAO-ATUAL.

       190-SEGUNDA-FASE.
      *----------------*

           MOVE   WRK-BVVE1665                 TO  PS-TRANSA.
           MOVE   IN-POOLMIL-AREA          TO  PS-POOLMIL-AREA.
           MOVE   IN-POOLMIL-OPC           TO  PS-POOLMIL-OPC.
           MOVE   IN-TIPO-PESQ-INV         TO  PS-TIPO-PESQ-INV.
           MOVE   IN-AGENCIA               TO  PS-AGENCIA.
           MOVE   IN-CONTA                 TO  PS-CONTA.

           IF  IN-PFK   EQUAL   '4'
ROMULO         MOVE   96         TO  PS-LL
               MOVE   BVVE1681   TO  MODNAME
           ELSE
ROMULO         MOVE   94         TO  PS-LL
               MOVE   BVVE1670   TO  MODNAME.

           MOVE  IN-NUM-CARTAO(IND)  TO  PS-CARTAO-REST.
           MOVE  IN-DIA(IND)         TO  PS-DIA.
           MOVE  IN-MES(IND)         TO  PS-MES.
ROM        MOVE  IN-SEC(IND)         TO  PS-SEC.
           MOVE  IN-ANO(IND)         TO  PS-ANO.
           MOVE  IN-SEG-VIA-INV      TO  PS-SEG-VIA-INV.

           IF  IN-DT-RECEBIDA   EQUAL   ZEROS
               IF  IN-CARTAO   EQUAL   ZEROS
                   MOVE  1                   TO  PS-MANEIRA-PESQ
               ELSE
                   MOVE  2                   TO  PS-MANEIRA-PESQ
           ELSE
               MOVE  3                   TO  PS-MANEIRA-PESQ.

           MOVE   CHNG                     TO  FUNCAO.
           MOVE   PS-MENSAGEM              TO  MENSAGEM.
           GO                        TO    020-PROCESSA-FUNCAO.

      /
       200-MONTA-CARTAO-ATUAL.
      *----------------------*



           MOVE  IN-PAGINA         TO  OU-PAGINA.

           COMPUTE   IND        =  13 * (IN-PAGINA - 1)


           IF  IN-DT-RECEBIDA   EQUAL   ZEROS

               IF  IN-CARTAO   EQUAL   ZEROS
                   GO   TO  210-DESPREZA-COM-C1-IND
               ELSE
                   GO   TO  230-DESPREZA-COM-C3-IND

           ELSE

               GO   TO  250-DESPREZA-COM-C2-IND.

      /
       210-DESPREZA-COM-C1-IND.
      *-----------------------*

           EXEC SQL
                OPEN  C1
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  19    TO      ERR-LOCAL
               MOVE  'OPEN'        TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

       220-FETCH-DESPREZO-C1-IND.
      *-------------------------*

           EXEC SQL
                FETCH   C1              INTO     :SEL-DT-FECH,
                                                 :SEL-NUM-CARTAO,
                                                 :SEL-AGENCIA,
                                                 :SEL-CONTA
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  20    TO      ERR-LOCAL
               MOVE  'FETCH'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

           IF  IND   EQUAL   0
               GO    TO    060-COMECA-A-LOTAR.

           SUBTRACT  1     FROM  IND.

           GO    TO    220-FETCH-DESPREZO-C1-IND.

      /
       230-DESPREZA-COM-C3-IND.
      *-----------------------*

           MOVE  IN-CARTAO         TO  SEL-NUM-CARTAO.

           EXEC SQL
                OPEN  C3
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  21    TO      ERR-LOCAL
               MOVE  'OPEN'        TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

       240-FETCH-DESPREZO-C3-IND.
      *-------------------------*

           EXEC SQL
                FETCH   C3              INTO     :SEL-DT-FECH,
                                                 :SEL-NUM-CARTAO,
                                                 :SEL-AGENCIA,
                                                 :SEL-CONTA
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  22    TO      ERR-LOCAL
               MOVE  'FETCH'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

           IF  IND   EQUAL   0
               GO    TO    100-COMECA-A-LOTAR-ESP.

           SUBTRACT  1     FROM  IND.

           GO    TO    240-FETCH-DESPREZO-C3-IND.


      /
       250-DESPREZA-COM-C2-IND.
      *-----------------------*

           STRING  IN-DD  '.'
                   IN-MM  '.'  IN-SS
                   IN-AA  DELIMITED BY SIZE
                   INTO SEL-DT-FECH

           EXEC SQL
                OPEN  C2
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  23    TO      ERR-LOCAL
               MOVE  'OPEN'        TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

       260-FETCH-DESPREZO-C2-IND.
      *-------------------------*

           EXEC SQL
                FETCH   C2              INTO     :SEL-DT-FECH,
                                                 :SEL-NUM-CARTAO,
                                                 :SEL-AGENCIA,
                                                 :SEL-CONTA
           END-EXEC.

           IF  SQLCODE   NOT EQUAL   ZEROS   OR
               SQLWARN0  EQUAL  'W'
               MOVE  24    TO      ERR-LOCAL
               MOVE  'FETCH'       TO  ERR-FUN-COMANDO
               GO    TO    999-DB2-ERROR.

           IF  IND   EQUAL   0
               GO    TO    140-COMECA-A-ENCHER.

           SUBTRACT  1     FROM  IND.

           GO    TO    260-FETCH-DESPREZO-C2-IND.

      /
       999-DB2-ERROR.
      *-------------*

           MOVE    'DB2'              TO    ERR-TIPO-ACESSO.
           MOVE    'KELO_FAT_ONLINE'  TO    ERR-DBD-TAB.
           MOVE    SQLCODE            TO    ERR-SQL-CODE.
           MOVE    SPACES             TO    ERR-SEGM.

       999-CALL7100.
      *------------*

           MOVE    USUARIO            TO    ERR-COD-USER.
           MOVE    DEPTO              TO    ERR-COD-DEPTO.
           MOVE    WRK-BVVE1665           TO    ERR-MODULO.

           CALL   'POOL7100' USING   IO-PCB  ERRO-AREA  ALT-PCB  SQLCA.
           GOBACK.
