      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. RUCA6630.
       AUTHOR. JAPI NOVEMBRO/06.
      *================================================================*
      *                                                                *
      *      PROGRAMA     : RUCA6630                                   *
      *      PROGRAMADOR  : JAPI                                       *
      *      SUPERVISOR   : JAPI                                       *
      *      ANALISTA     : JAPI                                       *
      *      DATA         : 05/11/2006                                 *
      *                                                                *
      *      OBJETIVO     : DEMONSTRAR SITUACAO DAS OPERACOES DO       *
      *                     CREDITO RURAL.                             *
      *                                                               *
      *      BANCO DE DADOS:                                          *
      *         DB2                                                   *
      *           TABLE                          INCLUDE/BOOK         *
      *           DB2PRD.RUCA_CAD_ROOT             RUCAB003           *
      *           DB2PRD.RUCA_CAD_DEP              RUCAB004           *
BR0118*           DB2PRD.TGRP_AGCLT_FAMLR          RURCB00D           *
7C0219*           DB2PRD.TLASTR_PCELA_AGRON_RISCO  RUCAB011           *
      *                                                               *
      *      TELAS                                                    *
      *      -----                                                    *
      *      RUCAT630                                                 *
      *      RUCAT631                                                 *
      *                                                               *
      *      MODULOS CHAMADOS                                         *
      *      ----------------                                         *
      *      - POOL0660 - INSERE CARACTER DE NULL NA MSG DE SAIDA     *
      *      - POOL7100 - TRATA ERROS DE APLICACAO                    *
      *      - POOL7600 - FORNECE DATA E HORA DO SISTEMA              *
      *      - POOL1210 - TRANSFORMA DT C/ SEC. DE GREGOR. P/ JULIAN. *
      *                                                               *
      *      NAVEGACAO                                                 *
      *      ---------                                                 *
      *      - SENH7020 - MENU DO RUCA                                 *
      *                                                                *
JAPI->*           DB   -  DB  DE  MOVTO                               *
JAPI->*           DB   -  DB  DO  CADASTRO                            *
JAPI->*                                                               *
JAPI-A*           TELA -  SENH7020 (MENU)                             *
JAPI-A*                   RUCAT630 (PGM)                              *
JAPI-A*                   RUCAT631 (PGM)                              *
JAPI-A*                   RUCAP630 (IMPRESSAO)                        *
JAPI-A*                   RUCAP631 (IMPRESSAO)                        *
JAPI->*                                                               *
JAPI->*---------------------------------------------------------------*
ACT115*                      A L T E R A C A O                        *
ACT115*===============================================================*
ACT115*                                                               *
ACT115*       ANALISTA...: MARINA FUJITA  - ALTRAN                    *
ACT115*       DATA.......: SETEMBRO/2007                              *
ACT115*       PROJETO....: ACT 07/0115-01  ANPR 2007/0865/5-00        *
ACT115*                    ALTERA REGRAS DE ENVIO RECOR/BACEN         *
ACT115*       OBJETIVO...: INCLUIR CAMPO DATA IMPRESSAO 4 VIAS        *
ACT115*                                                               *
ACT006*================================================================*
ACT006*               U L T I M A    A L T E R A C A O                 *
ACT006*                                                                *
ACT006*  ANALISTA     : LEANDRO LIMA   - BRAD / GP.70                  *
ACT006*  ANALISTA     : FABIO BARRILE  - ALTRAN                        *
ACT006*  DATA         : JUNHO/2008                                     *
ACT006*                                                                *
ACT006*  PROJETO      : ACT 08/0006-01  ANPR 2007/1854/5-00            *
ACT006*                 MELHORIAS BACEN                                *
ACT006*                                                                *
ACT006*  OBJETIVO(S)  : DESCONSIDERAR CONTRATOS FIMA NA TAB. RUFIV002  *
ACT006*                                                                *
BS0214*================================================================*
BS0214*                 A  L  T  E  R  A  C  A  O                     *
BS0214*---------------------------------------------------------------*
BS0214*                                                               *
BS0214*        ANALISTA : MARINA FUJITA - BSI TECNOLOGIA              *
BS0214*        DATA     : MARCO/2013                                  *
BS0214*        PROJETO  : BSI 12/0214  -  INTEGRACAO CRUR - LEGADO    *
BS0214*                   ANPR 2007/30/7-32                           *
BS0214*        OBJETIVO : AUMENTAR CONTRATO DE 7 PARA 9 DIGITOS.      *
BS0214*                                                               *
BS0214*===============================================================*
BS0915*                                                               *
BS0915*        ANALISTA : EDSON MATSUMOTO - BSI TECNOLOGIA            *
BS0915*        DATA     : SETEMBRO / 2015                             *
BS0915*        OBJETIVO : ALM 4626 CAMPO FINANC RURAL NUM  TIRAR A    *
BS0915*                   BARRA                                       *
BS0915*                                                               *
BS0915*===============================================================*
BR0118*---------------------------------------------------------------*
BR0118*                                                               *
BR0118*        ANALISTA : EDSON MATSUMOTO - BRQ SOLUTIONS             *
BR0118*        DATA     : JANEIRO / 2018                              *
BR0118*        OBJETIVO : TRATAR PGPAF                                *
BR0118*                                                               *
BR0118*===============================================================*
7C0219*---------------------------------------------------------------*
7C0219*                                                               *
7C0219*        ANALISTA : EDSON MATSUMOTO - 7COMM                     *
7C0219*        DATA     : FEVEREIRO / 2019                            *
7C0219*        OBJETIVO : INCLUIR CAMPO LASTRO                        *
7C0219*                                                               *
7C0219*===============================================================*
RO0419*        DATA     : ABRIL/2019                                  *
RO0419*        PROJETO  : AGRO - CONCILIACAO CORPORATIVA DE DOCUMENTOS*
RO0419*                   PARA CREDITO RURAL - COD BARRAS DINAMICO    *
RO0419*        OBJETIVO : IMPLEMENTACAO CODIGO DE BARRAS DINAMICO     *
RO0419*                   ACESSAR TABELA CMRU QUE POSSUO O CODIGO.    *
RO0419*===============================================================*
BR1020*===============================================================*
BR1020*        ANALISTA : EVANDRO GUIMARAES        - BRQ              *
BR1020*        DATA     : OUTUBRO/2020                                *
BR1020*        PROJETO  : MEGA DESVINCULO DE CONTAS                   *
BR1020*        OBJETIVO : NOVA PF - CONSULTA BOLETOS                  *
BR1020*===============================================================*
FSY319*     ANALISTA : LEVI ANTUNES PEREIRA    - FOURSYS               *
FSY319*     DATA     : JUNHO / 2021                                    *
FSY319*     OBJETIVO : AJUSTE NA FORMATACAO DA CARTEIRA PARA INCLUSAO  *
FSY319*                NA BASE DE DADOS OPTINS (FNEB)                  *
FSY319*     PROJETO  : 2020-0591960-5-003                              *
FSY319*================================================================*
WIP001*     ANALISTA : WIPRO              - WIPRO                      *
WIP001*     DATA     : DEZ / 2021                                      *
WIP001*     OBJETIVO : POSSIBILITAR CONSULTA DE DADOS DO ZONEAMENTO    *
WIP001*================================================================*
RUSSO1*================================================================*
RUSSO1*     ANALISTA : RODRIGO KONSTANTINOVAS - BRADESCO               *
RUSSO1*     DATA     : 29/02/2024                                      *
RUSSO1*     OBJETIVO : INCLUSAO DA PF6 PARA CHAMADA DA TRANSACAO       *
RUSSO1*                RUCA6690 - CONSULTA DO DESCRITIVO DE CREDITO DA *
RUSSO1*                OPERACAO                                        *
RUSSO1*================================================================*
7C2603*================================================================*
7C2603*     ANALISTA : HERIBERTO GIANNASI - 7COMM                      *
7C2603*     DATA     : 02/03/2026                                      *
7C2603*     OBJETIVO : INCLUSAO DA NAVEGACAO PF4                       *
7C2603*================================================================*

       ENVIRONMENT DIVISION.
      **********************

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ***************

       WORKING-STORAGE SECTION.

      ********   FUNCOES   DO   CALL   ********

       77  GU                          PIC X(04)         VALUE 'GU  '.
       77  GHU                         PIC X(04)         VALUE 'GHU '.
       77  GHNP                        PIC X(04)         VALUE 'GHNP'.
       77  CHNG                        PIC X(04)         VALUE 'CHNG'.
       77  ISRT                        PIC X(04)         VALUE 'ISRT'.
       77  REPL                        PIC X(04)         VALUE 'REPL'.
       77  PURG                        PIC X(04)         VALUE 'PURG'.
       77  ROLB                        PIC X(04)         VALUE 'ROLB'.

       77  WRK-ORIGEM-AUX              PIC 9(03)         VALUE ZEROS.
       77  WRK-SOMATORIA               PIC S9(16)V99      VALUE ZEROS.
BS0214 77  WRK-CCTRO-CUSTO-AUX         PIC X(04)         VALUE SPACES.
       77  WRK-DIGITO                  PIC X.
BS0214 77  WRK-TAMANHO                 PIC 99            VALUE 9.
       77  WRK-TAM                     PIC 99            VALUE 4.
       77  WRK-SAL                     PIC 9(13)V99 COMP-3 VALUE 0.
ECR    77  WRK-MODULO                  PIC X(08)           VALUE SPACES.

       01  WRK-CAMPOS-POOL5000.
           03  WRK-FUNCAO              PIC  X(04)        VALUE SPACES.
           03  WRK-MENSAGEM.
               05  FILLER              PIC  X(04)        VALUE SPACES.
               05  WRK-TRANSACAO       PIC  X(08)        VALUE SPACES.
               05  FILLER              PIC  X(1988)      VALUE SPACES.
           03  WRK-TELA                PIC  X(08)        VALUE SPACES.
           03  WRK-NOVA-OPCAO          PIC  9(02)        VALUE ZEROS.
           03  WRK-COD-USER            PIC  9(07)        VALUE ZEROS.
           03  WRK-OPCAO-ATUAL         PIC  9(02)        VALUE ZEROS.
           03  WRK-COD-DEPTO.
               05  FILLER              PIC  9(02)        VALUE ZEROS.
               05  WRK-COD-DEPTO-N     PIC  9(04)        VALUE ZEROS.

       01  ATR-LUMI                    PIC 9(5)   COMP   VALUE 49289.
       01  ATR-POS                     PIC 9(5)   COMP   VALUE 49281.
       01  ATR-MOD                     PIC 9(5)   COMP   VALUE 129.


       01  WRK-CTABANEB.
           03  WRK-AGEN-BANEB          PIC 9(07).
           03  WRK-CONT-BANEB          PIC 9(08).

       01  WRK-DIGBANEB.
           03  WRK-DIGI-BANEB          PIC X(01).

JAPI=I 01  WRK-IMPRESSORA              PIC X(8)          VALUE SPACES.

BR0118 01  WRK-RESUMO.
BR0118     03  WRK-RES1                PIC X             VALUE SPACES.
BR0118     03  WRK-RES2                PIC X             VALUE SPACES.
BR0118     03  WRK-RES3                PIC X             VALUE SPACES.
BR0118     03  WRK-RES4                PIC X             VALUE SPACES.
BR0118     03  WRK-RES5                PIC X             VALUE SPACES.
BR0118     03  WRK-RES6                PIC X             VALUE SPACES.
BR0118     03  WRK-RES7                PIC X             VALUE SPACES.
BR0118     03  WRK-RES8                PIC X             VALUE SPACES.
BR0118     03  WRK-RES9                PIC X             VALUE SPACES.
BR0118     03  WRK-RES10               PIC X             VALUE SPACES.
BR0118 01  WRK-GP.
BR0118     03  WRK-GP1                 PIC X             VALUE SPACES.
BR0118     03  WRK-GP2                 PIC X             VALUE SPACES.
BR0118     03  WRK-GP3                 PIC X             VALUE SPACES.

       01  WRK-DESTINO.
           03  WRK-DEST1               PIC X             VALUE SPACES.
           03  WRK-DEST2               PIC X(7)          VALUE SPACES.

       01  WRK-MODNAME                 PIC X(8)          VALUE SPACES.

       01  WRK-CONV                    PIC 9.
       01  WRK-BIT-BYTE                PIC 99.
       01  OITOBITS.
           03  BIT     OCCURS 8 TIMES  PIC X.
       01  WRK-CONVERTIDO              PIC X(40).

      ******** ESTA AREA SO ESTA SENDO CRIADA POIS A POOL0004 *******
      ******** ESTA INVANDINDO 4 BYTES E ASSIM DESLIGANDO     *******
      ******** BITS QUE DEVEM PERMANECER LIGADOS              *******

           05  FILLER                  PIC X(02).
           05  WRK-BYTE                PIC X(01).
           05  FILLER                  PIC X(10).

      ***************************************************************

       01  WRK-CGC.
           03  WRK-NUMCGC              PIC 9(9).
           03  WRK-FILIALCGC           PIC 9(5).

       01  WRK-DESC-CGC.
           03  WRK-NUMFILI             PIC 9(13).
           03  WRK-CONT                PIC 99.
       01  WRK-DESC-CGC-R  REDEFINES  WRK-DESC-CGC PIC 9(15).

       01  WRK-CONTROLE                PIC X(2).

BR0118 01  WRK-DATAS-C.
BR0118     03  WRK-DD-C                PIC 99     VALUE ZEROS.
BR0118     03  FILLER                  PIC X      VALUE SPACES.
BR0118     03  WRK-MM-C                PIC 99     VALUE ZEROS.
BR0118     03  WRK-AA-C                PIC 9999   VALUE ZEROS.

BR0118 01  WRK-DATAS-D.
BR0118     03  WRK-DD-D                PIC 99     VALUE ZEROS.
BR0118     03  WRK-MM-D                PIC 99     VALUE ZEROS.
BR0118     03  WRK-AA-D                PIC 9999   VALUE ZEROS.
JAPI=A 01  WRK-DATAS-D-R REDEFINES WRK-DATAS-D PIC 9(8).

       01  WRK-DATAS.
JAPI=A     03  WRK-AA                  PIC 9999.
           03  WRK-MM                  PIC 99.
           03  WRK-DD                  PIC 99.
JAPI=A 01  WRK-DATAS-R REDEFINES WRK-DATAS PIC 9(8).

       01  WRK-INV-DATAS.
           03  WRK-DD                  PIC 99.
           03  WRK-MM                  PIC 99.
JAPI=A     03  WRK-AA                  PIC 9999.
JAPI=A 01  WRK-INV-DATAS-R REDEFINES WRK-INV-DATAS PIC 9(8).


BS0915 01  FILLER  REDEFINES  WRK-CAMPO-9.
BS0915     03  FILLER              PIC  9(02).
BS0915     03  WRK-CAMPO-7         PIC  9(07).

FSY319 01  WRK-CART-X                      PIC  X(005) VALUE ZEROS.
FSY319 01  WRK-CART-N REDEFINES WRK-CART-X PIC  9(005).
FSY319 01  WRK-CART-P                      PIC  9(005)
FSY319                                     COMP-3   VALUE ZEROS.

      ********   MENSAGEM  DE  INCONSISTENCIA   ********

       01  MENS-INC.
           03  FILLER                  PIC X(28)         VALUE '*** DADO
      -        'S INCONSISTENTES ***'.


      ********   MENSAGEM  DE  REGISTRO  NAO  ENCONTRADO   ********

       01  MENS-REGNENC.
           03  FILLER                  PIC X(31)         VALUE '*** REGI
      -        'STRO NAO ENCONTRADO ***'.
      ********   MENSAGEM CONTRATO NAO CADASTRADO NA RUFIV002 ********
       01  MENS-RUFIV002.
           03  FILLER                  PIC X(46)         VALUE '*** REGI
      -        'STRO NAO ENCONTRADO - RUFIV002 ***'.

      ********   MENSAGEM CONTRATO NAO CADASTRADO NA RUFIV002 ********
       01  MENS-RUCAB003.
           03  FILLER                  PIC X(46)         VALUE '*** REGI
      -        'STRO NAO ENCONTRADO - RUCAB003 ***'.

      ********   MENSAGEM  DE  PED. DE SIT. INCLUIDO   ********

       01  MENS-SITINC.
           03  FILLER                  PIC X(35)         VALUE '*** PEDI
      -        'DO DE SITUACAO INCLUIDO ***'.

      ********   MENSAGEM  DE  IMPRESSORA  NAO  DISPONIVEL   **********

       01  MENS-IMPNDISP.
           03  FILLER                  PIC X(33)         VALUE '*** IMPR
      -        'ESSORA NAO DISPONIVEL ***'.
      ********   MENSAGEM  DE  TECLE     ENTER               **********

       01  MENS-ENTER.
           03  FILLER                  PIC X(07)         VALUE SPACES.
           03  FILLER                  PIC X(43)         VALUE '*** TECL
      -        'E ENTER PARA CONTINUAR ***'.

      ********   MENSAGEM CONTRATO NAO EH DO BANEB            ********
       01  MENS-BANEB.
           03  FILLER                  PIC X(46)         VALUE '*** REGI
      -        'STRO NAO ENCONTRADO ***'.


JAPI=A********   TELA  DE  ENTRADA   -   RUCAT630       ********

       01  IN-TELA630.
           03  FILLER                  PIC X(18).
           03  IN-PF630                PIC X(01).
           03  IN-DADOS630.
               05  IN-SENHAS630        PIC X(37).
               05  IN-NOPCAO630        PIC X(02).
BS0214             10  WRK-IN-CHAVE630 PIC 9(09) VALUE ZEROS.
                   10  IN-DIG630       PIC X(01).
                   10  IN-ORDEM630     PIC 9(03).
                   10  IN-S-N630       PIC X(01).
WIP001         05  FILLER              PIC X(97) VALUE SPACES.
           03  IN-DADOS631 REDEFINES IN-DADOS630.
               05  IN-SENHAS631        PIC X(37).
               05  IN-NOPCAO631        PIC X(02).
BS0214         05  WRK-IN-CHAVE631     PIC 9(09).
               05  IN-ORDEM631         PIC 9(03).
               05  IN-IND631           PIC X(01).
BR1020         05  WRK-I-NROPER        PIC X(09).
BR1020         05  WRK-I-AGENCIA       PIC X(05).
BR1020         05  WRK-I-NOMAG         PIC X(20).
BR1020         05  WRK-I-AAFINAN       PIC X(08).
BR1020         05  WRK-I-NOME          PIC X(30).
BR1020         05  WRK-I-CC            PIC X(09).
BR1020         05  WRK-I-CGCCPF        PIC X(17).
7C2603         05  WRK-I-ORIREC        PIC 9(03).

JAPI=A********   TELA  DE  SAIDA   -   RUCAT630         ********
       01  OUT-TELA630.
           03  OUT-LL630               PIC 9(4)  COMP    VALUE 115.
           03  OUT-ZZ630               PIC 9(4)  COMP    VALUE ZEROS.
           03  OUT-DADOS630.
               05  OUT-SENHAS630       PIC X(37).
               05  OUT-NOPCAO630       PIC X(02).
               05  OUT-ATR-CHA630      PIC 9(04)  COMP.
BS0214         05  WRK-OUT-CHAVE630    PIC X(09) VALUE SPACES.
HE0609         05  WRK-OUT-CHAVE630-N REDEFINES WRK-OUT-CHAVE630
BS0214                                 PIC 9(09).
               05  OUT-ATR-DIG630      PIC 9(04)  COMP.
               05  OUT-DIG630          PIC X(01).
               05  OUT-ATR-ORD630      PIC 9(04)  COMP.
               05  OUT-ORDEM630        PIC X(03).
HE0609         05  WRK-OUT-ORDEM630-N REDEFINES OUT-ORDEM630
HE0609                                 PIC 9(03).
               05  OUT-ATR-S-N630      PIC 9(04)  COMP.
               05  OUT-S-N630          PIC X(01).
               05  OUT-MENSAGEM630     PIC X(50).

JAPI=A********   MASCARA  DAS  TELAS  RUCAT630          ********
JAPI=A 01  MASC-RUCAT630.
           03  660-LL-AREA-T630        PIC  9(04)  COMP  VALUE 0018.
BS0214     03  660-LL-MENSAGEM-T630    PIC  9(04)  COMP  VALUE 0115.
           03  660-SENHAS-T630         PIC  9(04)  COMP  VALUE 0037.
           03  660-NOPCAO-T630         PIC  9(04)  COMP  VALUE 0002.
BS0214     03  660-CHAVE-T630          PIC  9(04)  COMP  VALUE 3011.
           03  660-DIG-T630            PIC  9(04)  COMP  VALUE 3003.
           03  660-ORDEM-T630          PIC  9(04)  COMP  VALUE 3005.
           03  660-LISTAR-T630         PIC  9(04)  COMP  VALUE 3003.
           03  660-MENSA-T630          PIC  9(04)  COMP  VALUE 0050.

JAPI=A********   TELA  DE  SAIDA   -   RUCAT631   ********

       01  OUT-TELA631.
POL128**   03  OUT-LL631               PIC 9(4)  COMP    VALUE 600.
7C0219**   03  WRK-OUT-LL631           PIC 9(4)  COMP    VALUE 652.
RO0419**   03  WRK-OUT-LL631           PIC 9(4)  COMP    VALUE 696.
RO0419     03  WRK-OUT-LL631           PIC 9(4)  COMP    VALUE 717.
           03  OUT-ZZ631               PIC 9(4)  COMP    VALUE ZEROS.
           03  OUT-DADOS631.
               05  OUT-SENHAS631       PIC X(37).
BS0214         05  OUT-CHAVE631        PIC 9(09) VALUE ZEROS.
               05  OUT-ORDEM631        PIC 9(03).
               05  OUT-IND631          PIC X(01).
               05  OUT-EXTRATO631      PIC 9(07).
               05  OUT-DIGEXT631       PIC X.
BS0214         05  OUT-CHAVEOPER631    PIC 9(09) VALUE ZEROS.
               05  OUT-DIGCHAVE631     PIC X.
ECR            05  WRK-OUT-FORMA-PGTO  PIC X(15)  VALUE SPACES.
ECR            05  WRK-OUT-DEBI        PIC X(03)  VALUE SPACES.
ECR            05  WRK-OUT-LIMI        PIC X(03)  VALUE SPACES.
               05  OUT-AG631           PIC 9(05).
               05  OUT-NOMEAG631       PIC X(20).
BS0915         05  OUT-FINAN631        PIC 9999999 VALUE ZEROS.
RO0418         05  OUT-CDBARR631       PIC X(44)  VALUE ZEROS.
               05  OUT-NOME631         PIC X(30).
               05  OUT-CONTACOR631     PIC 9(07)/.
               05  OUT-DIGCONTA631     PIC X.
               05  OUT-CGC-CPF631      PIC 999999999/9999.
               05  OUT-HIF631          PIC X              VALUE  '-'.
               05  OUT-CONTROLE631     PIC 99.
               05  OUT-TIPROD631       PIC 99.
               05  OUT-CAPACID631      PIC ZZZZZZZZZ9.
BR0118         05  WRK-OUT-NDAP631     PIC X(25) VALUE SPACES.
BR0118         05  WRK-OUT-GRUPO631    PIC X(03) VALUE SPACES.
BR0118         05  WRK-OUT-DTVALID631  PIC 99/99/9999 VALUE ZEROS.
               05  OUT-ATIV631         PIC 9(03).
               05  OUT-EMBAL631        PIC 9(03).
               05  OUT-FISCAL631       PIC ZZZZZZZZZ9.
               05  OUT-FINALID631      PIC 9(03).
               05  OUT-ORIGREC631      PIC 9(03).
               05  OUT-CLAS631         PIC ZZZZZZZZZ9.
               05  OUT-EMPREE631       PIC 9(03).
               05  OUT-CONV631         PIC 9(03).
POL128         05  OUT-SEGR631         PIC 9(03).
               05  OUT-RPL631          PIC 9.
               05  OUT-TPTIT631        PIC 9(03).
               05  OUT-TPGAR631        PIC 9(03).
ACT115         05  WRK-OUT-DIMPRE631   PIC X(10)          VALUE SPACES.
               05  OUT-AREATOT631      PIC ZZ.ZZZ.ZZZ.ZZ9,99.
               05  OUT-QTDFIN631       PIC ZZ.ZZZ.ZZZ.ZZ9,99.
               05  OUT-UNID631         PIC 9(04).
               05  OUT-CGCINST631      PIC X(11).
               05  OUT-CONTRINST631    PIC 99.
               05  OUT-LITSEC20631     PIC X(25).
               05  OUT-NUMBCO631       PIC 9(08)B/B9.
PW9931         05  WRK-OUT-NUMBCO631 REDEFINES OUT-NUMBCO631
PW9931                                 PIC X(12).
               05  OUT-CODMUNI631      PIC 9(07).
7C0219         05  WRK-OUT-LASTRO631   PIC X(01)          VALUE SPACES.
JAPI=A         05  OUT-DTREG631        PIC 99/99/9999.
JAPI=A         05  OUT-DTCED631        PIC 99/99/9999.
JAPI=A         05  OUT-DTVCTOPRO631    PIC 99/99/9999.
JAPI=A         05  OUT-DTVCTO631       PIC 99/99/9999.
               05  OUT-SLDDEV631       PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  OUT-LITER1631       PIC X(26).
               05  OUT-VRSECUR         PIC ZZZ.ZZZ.ZZ9,99.
               05  OUT-TOTORC631       PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  OUT-LITERA631       PIC X(19).
               05  OUT-VRCOB631        PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  OUT-TOTFIN631       PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  OUT-VRREC631        PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  OUT-CABEC-T631      PIC X(26).
               05  OUT-MENSAGEM631     PIC X(50)         VALUE SPACES.
JAPI=A********   MASCARA  DA  TELA  RUCAT631   ********

JAPI=A 01  MASC-RUCAT631.
POL128**   03  660-LL-AREA-T631        PIC  9(04)  COMP  VALUE 0110.
7C0219**   03  WRK-660-LL-AREA-T631    PIC  9(04)  COMP  VALUE 0122.
ECR        03  WRK-660-LL-AREA-T631    PIC  9(04)  COMP  VALUE 0128.
POL128**   03  660-LL-MENSAGEM-T631    PIC  9(04)  COMP  VALUE 0600.
7C0219**   03  WRK-660-LL-MENSAGEM-T631 PIC  9(04)  COMP  VALUE 0652.
RO0419**   03  WRK-660-LL-MENSAGEM-T631 PIC  9(04)  COMP  VALUE 0696.
ECR        03  WRK-660-LL-MENSAGEM-T631 PIC  9(04)  COMP  VALUE 0717.
           03  660-SENHAS-T631         PIC  9(04)  COMP  VALUE 0037.
           03  660-NOPCAO-T631         PIC  9(04)  COMP  VALUE 0002.
BS0214     03  660-CHAVE-T631          PIC  9(04)  COMP  VALUE 0009.
           03  660-ORDEM-T631          PIC  9(04)  COMP  VALUE 0003.
           03  660-IND-T631            PIC  9(04)  COMP  VALUE 0001.
           03  660-CCVINC-T631         PIC  9(04)  COMP  VALUE 0007.
           03  660-DIGCCV-T631         PIC  9(04)  COMP  VALUE 0001.
BS0214     03  660-NROPER-T631         PIC  9(04)  COMP  VALUE 0009.
           03  660-DIGOPER-T631        PIC  9(04)  COMP  VALUE 0001.
ECR        03  WRK-660-FORMA-T631      PIC  9(04)  COMP  VALUE 0015.
ECR        03  WRK-660-DEBI-T631       PIC  9(04)  COMP  VALUE 0003.
           03  660-AGENCIA-T631        PIC  9(04)  COMP  VALUE 0005.
           03  660-NOMAG-T631          PIC  9(04)  COMP  VALUE 0020.
BS0915     03  660-AAFINAN-T631        PIC  9(04)  COMP  VALUE 0007.
RO0419     03  660-CDBARR-T631         PIC  9(04)  COMP  VALUE 0044.
           03  660-NOME-T631           PIC  9(04)  COMP  VALUE 0030.
           03  660-CC-T631             PIC  9(04)  COMP  VALUE 0009.
           03  660-CGCCPF-T631         PIC  9(04)  COMP  VALUE 0017.
           03  660-TIPROD-T631         PIC  9(04)  COMP  VALUE 0002.
           03  660-CATMUT-T631         PIC  9(04)  COMP  VALUE 0002.
           03  660-CAPROD-T631         PIC  9(04)  COMP  VALUE 0010.
BR0118     03  WRK-660-NDAP631         PIC  9(04)  COMP  VALUE 0025.
BR0118     03  WRK-660-GRUPO631        PIC  9(04)  COMP  VALUE 0003.
BR0118     03  WRK-660-DTVALID631      PIC  9(04)  COMP  VALUE 0010.
           03  660-ATIVID-T631         PIC  9(04)  COMP  VALUE 0003.
           03  660-EMBALA-T631         PIC  9(04)  COMP  VALUE 0003.
           03  660-FISCAL-T631         PIC  9(04)  COMP  VALUE 0010.
           03  660-FINALID-T631        PIC  9(04)  COMP  VALUE 0003.
           03  660-ORIREC-T631         PIC  9(04)  COMP  VALUE 0003.
           03  660-CLAPRO-T631         PIC  9(04)  COMP  VALUE 0010.
           03  660-EMPREEN-T631        PIC  9(04)  COMP  VALUE 0003.
           03  660-CFSEG-T631          PIC  9(04)  COMP  VALUE 0003.
           03  660-CP100-T631          PIC  9(04)  COMP  VALUE 0001.
           03  660-TPTIT-T631          PIC  9(04)  COMP  VALUE 0003.
           03  660-TPGAR-T631          PIC  9(04)  COMP  VALUE 0003.
ACT115     03  WRK-660-DIMPRE-T631     PIC  9(04)  COMP  VALUE 0010.
           03  660-AREA-T631           PIC  9(04)  COMP  VALUE 0017.
           03  660-QTFIN-T631          PIC  9(04)  COMP  VALUE 0017.
           03  660-UNID-T631           PIC  9(04)  COMP  VALUE 0004.
           03  660-CGCINST-T631        PIC  9(04)  COMP  VALUE 0011.
           03  660-FILINST-T631        PIC  9(04)  COMP  VALUE 0004.
           03  660-CTRINST-T631        PIC  9(04)  COMP  VALUE 0002.
           03  660-LITSEC20-T631       PIC  9(04)  COMP  VALUE 0025.
           03  660-NUMREF-T631         PIC  9(04)  COMP  VALUE 0012.
           03  660-CODMUN-T631         PIC  9(04)  COMP  VALUE 0007.
7C0219     03  WRK-660-LASTRO-T631     PIC  9(04)  COMP  VALUE 0001.
JAPI=A     03  660-DTREG-T631          PIC  9(04)  COMP  VALUE 0010.
JAPI=A     03  660-DTOPE-T631          PIC  9(04)  COMP  VALUE 0010.
JAPI=A     03  660-DTCED-T631          PIC  9(04)  COMP  VALUE 0010.
JAPI=A     03  660-VFINAL-T631         PIC  9(04)  COMP  VALUE 0010.
           03  660-DEVEDOR-T631        PIC  9(04)  COMP  VALUE 0021.
           03  660-LITER1-T631         PIC  9(04)  COMP  VALUE 0026.
           03  660-VRSECUR-T631        PIC  9(04)  COMP  VALUE 0014.
           03  660-LITERA-T631         PIC  9(04)  COMP  VALUE 0019.
           03  660-COBERT-T631         PIC  9(04)  COMP  VALUE 0021.
           03  660-CREDITO-T631        PIC  9(04)  COMP  VALUE 0021.
           03  660-RECPRO-T631         PIC  9(04)  COMP  VALUE 0021.
           03  660-CABEC-T631          PIC  9(04)  COMP  VALUE 0026.
           03  660-MENSAGEM-T631       PIC  9(04)  COMP  VALUE 0050.

JAPI=A********   TELA  DE  IMPRESSAO   -   RUCAP631   ********

       01  IMP-TELA631.
POL128**   03  IMP-LL631               PIC 9(4)  COMP    VALUE 541.
7C0219     03  WRK-IMP-LL631           PIC 9(4)  COMP    VALUE 635.
           03  IMP-ZZ631               PIC 9(4)  COMP    VALUE ZEROS.
           03  IMP-DADOS631.
               05  IMP-CABEC631        PIC X(34).
               05  IMP-ESCOND631       PIC X(33).
               05  IMP-EXTRATO631      PIC 9(7).
               05  IMP-DIGEXT631       PIC X.
BS0214         05  WRK-IMP-CHAVEOPER631 PIC 9(9) VALUE ZEROS.
               05  IMP-DIGCHAVE631     PIC X.
               05  IMP-AG631           PIC 9(5).
BS0915         05  IMP-FINAN631        PIC 9999999 VALUE ZEROS.
RO0419         05  WRK-IMP-CDBARR      PIC X(44)   VALUE ZEROS.
               05  IMP-NOME631         PIC X(30).
               05  IMP-CONTACOR631     PIC 9(7)/.
               05  IMP-DIGCONTA631     PIC X.
               05  IMP-CGC-CPF631      PIC 999999999/9999.
               05  IMP-HIF631          PIC X               VALUE '-'.
               05  IMP-CONTROLE631     PIC 99.
               05  IMP-TIPROD631       PIC 99.
               05  IMP-CATEGORIA631    PIC Z9.
               05  IMP-CAPACID631      PIC ZZZZZZZZZ9.
BR0118         05  WRK-IMP-NDAP631     PIC X(25) VALUE SPACES.
BR0118         05  WRK-IMP-GRUPO631    PIC X(03) VALUE SPACES.
BR0118         05  WRK-IMP-DTVALID631  PIC 99/99/9999 VALUE ZEROS.
               05  IMP-ATIV631         PIC 9(3).
               05  IMP-EMBAL631        PIC 9(3).
               05  IMP-FISCAL631       PIC ZZZZZZZZZ9.
               05  IMP-FINALID631      PIC 9(3).
               05  IMP-ORIGREC631      PIC 9(3).
               05  IMP-CLAS631         PIC ZZZZZZZZZ9.
               05  IMP-EMPREE631       PIC 9(3).
POL128         05  IMP-SEGR631         PIC 9(3).
               05  IMP-RPL631          PIC 9.
               05  IMP-TPTIT631        PIC 9(03).
               05  IMP-TPGAR631        PIC 9(03).
ACT115         05  WRK-IMP-DIMPRE631   PIC X(10)        VALUE SPACES.
               05  IMP-AREATOT631      PIC ZZ.ZZZ.ZZZ.ZZ9,99.
               05  IMP-QTDFIN631       PIC ZZ.ZZZ.ZZZ.ZZ9,99.
               05  IMP-UNID631         PIC 9(4).
               05  IMP-CGCINST631      PIC X(11).
               05  IMP-FILINST631      PIC 9999.
               05  IMP-CONTRINST631    PIC 99.
               05  IMP-LITSEC20631     PIC X(25).
               05  IMP-NUMBCO631       PIC 9(8)B/B9.
PW9931         05  WRK-IMP-NUMBCO631 REDEFINES IMP-NUMBCO631
PW9931                                 PIC X(12).
               05  IMP-CODMUNI631      PIC 9(7).
7C0219         05  WRK-IMP-LASTRO631   PIC X(01)        VALUE SPACES.
JAPI=A         05  IMP-DTREG631        PIC 99/99/9999.
JAPI=A         05  IMP-DTCED631        PIC 99/99/9999.
JAPI=A         05  IMP-DTVCTOPRO631    PIC 99/99/9999.
JAPI=A         05  IMP-DTVCTO631       PIC 99/99/9999.
               05  IMP-LITER1631       PIC X(26).
               05  IMP-VRSECUR         PIC ZZZ.ZZZ.ZZZ,99.
               05  IMP-TOTORC631       PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  IMP-LITERA631       PIC X(19).
               05  IMP-VRCOB631        PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  IMP-TOTFIN631       PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
               05  IMP-VRREC631        PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.

JAPI=A********   MASCARA  DA  TELA  RUCAP631   ********

JAPI=A 01  MASC-RUCAP631.
POL128**   03  660-LL-AREA-P631        PIC  9(04)  COMP  VALUE 0100.
7C0219     03  WRK-660-LL-AREA-P631    PIC  9(04)  COMP  VALUE 0112.
POL128**   03  660-LL-MENSAGEM-P631    PIC  9(04)  COMP  VALUE 0541.
7C0219     03  WRK-660-LL-MENSAGEM-P631 PIC  9(04)  COMP  VALUE 0635.
           03  660-CABEC-P631          PIC  9(04)  COMP  VALUE 1034.
           03  660-ESCOND-P631         PIC  9(04)  COMP  VALUE 1033.
           03  660-CCVINC-P631         PIC  9(04)  COMP  VALUE 1007.
           03  660-DIGCCV-P631         PIC  9(04)  COMP  VALUE 1001.
BS0214     03  660-NROPER-P631         PIC  9(04)  COMP  VALUE 1009.
           03  660-DIGOPER-P631        PIC  9(04)  COMP  VALUE 1001.
RO0419     03  WRK-660-CDBARR-P631     PIC  9(04)  COMP  VALUE 1044.
           03  660-NOMAG-P631          PIC  9(04)  COMP  VALUE 1020.
BS0915     03  660-AAFINAN-P631        PIC  9(04)  COMP  VALUE 1007.
           03  660-NOME-P631           PIC  9(04)  COMP  VALUE 1030.
           03  660-CC-P631             PIC  9(04)  COMP  VALUE 1009.
           03  660-CGCCPF-P631         PIC  9(04)  COMP  VALUE 1017.
           03  660-TIPROD-P631         PIC  9(04)  COMP  VALUE 1002.
           03  660-CATMUT-P631         PIC  9(04)  COMP  VALUE 1002.
           03  660-CAPROD-P631         PIC  9(04)  COMP  VALUE 1010.
BR0118     03  WRK-660-NDAP-P631       PIC  9(04)  COMP  VALUE 1025.
BR0118     03  WRK-660-GRUPO-P631      PIC  9(04)  COMP  VALUE 1003.
BR0118     03  WRK-660-DTVALID-P631    PIC  9(04)  COMP  VALUE 1010.
           03  660-ATIVID-P631         PIC  9(04)  COMP  VALUE 1003.
           03  660-EMBALA-P631         PIC  9(04)  COMP  VALUE 1003.
           03  660-FISCAL-P631         PIC  9(04)  COMP  VALUE 1010.
           03  660-FINALID-P631        PIC  9(04)  COMP  VALUE 1003.
           03  660-ORIREC-P631         PIC  9(04)  COMP  VALUE 1003.
           03  660-CLAPRO-P631         PIC  9(04)  COMP  VALUE 1010.
           03  660-EMPREEN-P631        PIC  9(04)  COMP  VALUE 1003.
           03  660-CFSEG-P631          PIC  9(04)  COMP  VALUE 1003.
POL128     03  660-SEGUR-P631          PIC  9(04)  COMP  VALUE 1003.
           03  660-TPTIT-P631          PIC  9(04)  COMP  VALUE 1003.
           03  660-TPGAR-P631          PIC  9(04)  COMP  VALUE 1003.
ACT115     03  WRK-660-DIMPRE-P631     PIC  9(04)  COMP  VALUE 1010.
           03  660-AREA-P631           PIC  9(04)  COMP  VALUE 1017.
           03  660-QTFIN-P631          PIC  9(04)  COMP  VALUE 1017.
           03  660-UNID-P631           PIC  9(04)  COMP  VALUE 1004.
           03  660-CGCINST-P631        PIC  9(04)  COMP  VALUE 1011.
           03  660-FILINST-P631        PIC  9(04)  COMP  VALUE 1004.
           03  660-CTRINST-P631        PIC  9(04)  COMP  VALUE 1002.
           03  660-LITSEC20-P631       PIC  9(04)  COMP  VALUE 1025.
           03  660-NUMREF-P631         PIC  9(04)  COMP  VALUE 1012.
           03  660-CODMUN-P631         PIC  9(04)  COMP  VALUE 1007.
7C0219     03  WRK-660-LASTRO-P631     PIC  9(04)  COMP  VALUE 1001.
JAPI=A     03  660-DTREG-P631          PIC  9(04)  COMP  VALUE 1010.
JAPI=A     03  660-DTOPE-P631          PIC  9(04)  COMP  VALUE 1010.
JAPI=A     03  660-DTCED-P631          PIC  9(04)  COMP  VALUE 1010.
JAPI=A     03  660-VFINAL-P631         PIC  9(04)  COMP  VALUE 1010.
           03  660-DEVEDOR-P631        PIC  9(04)  COMP  VALUE 1021.
           03  660-LITER1-P631         PIC  9(04)  COMP  VALUE 1026.
           03  660-VRSECUR-P631        PIC  9(04)  COMP  VALUE 1014.
           03  660-ORCADO-P631         PIC  9(04)  COMP  VALUE 1021.
           03  660-COBERT-P631         PIC  9(04)  COMP  VALUE 1021.
           03  660-CREDITO-P631        PIC  9(04)  COMP  VALUE 1021.
           03  660-RECPRO-P631         PIC  9(04)  COMP  VALUE 1021.

JAPI=A********   TELA  DE  IMPRESSAO   -   RUCAP630   ********

       01  IMP-TELA630.
BS0214     03  WRK-IMP-LL630           PIC 9(4)  COMP    VALUE 72.
           03  IMP-ZZ630               PIC 9(4)  COMP    VALUE ZEROS.
           03  IMP-DADOS630.
               05  IMP-MENSAGEM630     PIC X(50).
BS0214         05  WRK-IMP-CHAVE630    PIC 9(9) VALUE ZEROS.
               05  IMP-DIG630          PIC X.
               05  IMP-ORDEM630        PIC 9(3).
               05  IMP-S-N630          PIC X.
               05  IMP-OPCAO630        PIC XX.

JAPI=A********   MASCARA  DA  TELA  RUCAP630   ********

JAPI=A 01  MASC-RUCAP630.
           03  660-LL-AREA-P630        PIC  9(04)  COMP  VALUE 0016.
           03  660-MSGPGM-P630         PIC  9(04)  COMP  VALUE 1050.
BS0214     03  660-NUMER-P630          PIC  9(04)  COMP  VALUE 1009.
           03  660-DIGNUM-P630         PIC  9(04)  COMP  VALUE 1001.
           03  660-ORDEM-P630          PIC  9(04)  COMP  VALUE 1003.
           03  660-LISTAR-P630         PIC  9(04)  COMP  VALUE 1001.
           03  660-OPCAO-P630          PIC  9(04)  COMP  VALUE 1002.

      ********   IO-AREA  DB  CADASTRO  -  SEGMENTO  RURA020   ********

       01  DB-CAD.
POL128     03  CAD-SEGURADORA          PIC 9(3)  COMP-3.
POL128     03  FILLER                  PIC X(1).
JAPI=A     03  CAD-CHAVE               PIC 9(9)  COMP-3.
           03  FILLER                  PIC X(2).
           03  CAD-CONTROLE.
               05  FILLER              PIC XX.
               05  CAD-BYTE            PIC X.
               05  FILLER              PIC X(13).
           03  CAD-CONTROLE-R REDEFINES CAD-CONTROLE.
               05  FILLER              PIC X(07).
               05  CAD-BYTE-2          PIC X.
           03  CAD-CONTASCC            PIC 9(7)  COMP-3.
           03  CAD-CONTASVINC          PIC 9(7)  COMP-3.
           03  CAD-NOMEMUT             PIC X(40).
           03  FILLER                  PIC X.
           03  CAD-CGCCPF1             PIC 9(15) COMP-3.
JAPI=A     03  CAD-PROD                PIC XX.
           03  FILLER                  PIC X(41).
           03  CAD-CGCCPF2             PIC 9(15) COMP-3.
           03  FILLER                  PIC X(49).
           03  CAD-AG                  PIC 9(5)  COMP-3.
JAPI=A     03  CAD-NUMEMP              PIC 9(9)  COMP-3.
           03  FILLER                  PIC X(6).
           03  CAD-LOCMUN              PIC 9(13) COMP-3.
JAPI=A     03  FILLER                  PIC X(23).
           03  CAD-AREATOT             PIC 9(13)V99 COMP-3.
JAPI=A     03  FILLER                  PIC X(01).
           03  CAD-NOMIMOVEL           PIC X(40).
           03  CAD-FIANCA              PIC 9(3)  COMP-3.
           03  FILLER                  PIC X(57).
           03  CAD-TITULO              PIC 9(03) COMP-3.
           03  CAD-GARANTIA            PIC 9(03) COMP-3.
JAPI=A     03  CAD-VLBENS              PIC 9(13)V99 COMP-3.
           03  FILLER                  PIC X(20).
           03  CAD-ORIRECCOD           PIC 9(3)  COMP-3.
           03  CAD-ORIRECRPL           PIC X.
JAPI=A     03  FILLER                  PIC X(5).
           03  CAD-ORIREC1COD          PIC 9(3)  COMP-3.
           03  CAD-ORIREC1RPL          PIC X.
JAPI=A     03  FILLER                  PIC X(5).
           03  CAD-ORIREC2COD          PIC 9(3)  COMP-3.
           03  CAD-ORIREC2RPL          PIC X.
JAPI=A     03  FILLER                  PIC X(5).
           03  CAD-ORIREC3COD          PIC 9(3)  COMP-3.
           03  CAD-ORIREC3RPL          PIC X.
JAPI=A     03  FILLER                  PIC X(5).
           03  CAD-NUMRECOR            PIC 9(9)  COMP-3.
JAPI=A     03  FILLER                  PIC X(25).
JAPI=A     03  CAD-FISCALIZACAO        PIC XX.
           03  CAD-ATIVIDADE           PIC 9(3)  COMP-3.
           03  FILLER                  PIC X(2).
           03  CAD-FINALIDADE          PIC 9(3)  COMP-3.
           03  CAD-EMPREEND            PIC 9(3)  COMP-3.
           03  CAD-UNIDADE             PIC 9(3)  COMP-3.
JAPI=A     03  CAD-PRODPROV            PIC 9(15) COMP-3.
JAPI=A     03  CAD-UNIDADE2            PIC 9(3)  COMP-3.
JAPI=A     03  FILLER                  PIC X(02).
JAPI=A     03  CAD-SAFRA-INI           PIC 9(07) COMP-3.
JAPI=A     03  CAD-SAFRA-FIM           PIC 9(07) COMP-3.
           03  CAD-TOTFIN              PIC 9(13)V99 COMP-3.
           03  CAD-RECPRO              PIC 9(13)V99 COMP-3.
JAPI=A     03  FILLER                  PIC X(10).
JAPI=A     03  CAD-CODINCID            PIC X(2).
JAPI=A     03  CAD-DTVCTOFIN           PIC 9(9)  COMP-3.
JAPI=A     03  CAD-DTCEDULA            PIC 9(9)  COMP-3.
JAPI=A     03  CAD-DTOPERACAO          PIC 9(9)  COMP-3.
JAPI=A     03  CAD-DTREG               PIC 9(9)  COMP-3.
           03  CAD-VRCOBPRO            PIC 9(13)V99 COMP-3.
           03  FILLER                  PIC X(8).
           03  CAD-CLASSPROD           PIC 9(5)  COMP-3.
           03  CAD-CODEMB              PIC 9(3)  COMP-3.
JAPI=A     03  CAD-CATMUT              PIC XXX.
           03  CAD-CAPPRO              PIC 9(11) COMP-3.
JAPI=A     03  CAD-EGFOUTROS           PIC 9(11) COMP-3.
           03  CAD-SDO-PRINCIP999      PIC S9(13)V99 COMP-3.
           03  CAD-SDO-PRINCPG999      PIC S9(13)V99 COMP-3.
           03  FILLER                  PIC X(048).
           03  CAD-SDO-DEVEDOR999      PIC S9(13)V99 COMP-3.
           03  CAD-SDO-JUROS999        PIC S9(13)V99 COMP-3.
           03  CAD-SDO-JURPG999        PIC S9(13)V99 COMP-3.
           03  CAD-SDO-CORRECAO999     PIC S9(13)V99 COMP-3.
           03  CAD-SDO-CORREPAG999     PIC S9(13)V99 COMP-3.
           03  FILLER                  PIC X(160).
           03  CAD-SDO-DEVEDOR998      PIC S9(13)V99 COMP-3.
           03  FILLER                  PIC X(008).
           03  CAD-SDO-JUROS998        PIC S9(13)V99 COMP-3.
           03  FILLER                  PIC X(08).
           03  CAD-SDO-CORRECAO998     PIC S9(13)V99 COMP-3.
           03  FILLER                  PIC X(56).
           03  CAD-SDO-VARIACAO998     PIC S9(13)V99 COMP-3.
POL128     03  XXX-SEGURADORA          PIC 9(3)  COMP-3.
POL128     03  FILLER                  PIC X(30).

JAPI=I 01  DB-CAD-R.
           02  S025-CH-TIPO            PIC 9(03)    COMP-3.
           02  S025-ESPECIE            PIC X.
           02  S025-CONTROLE.
               03 S025-CONTROL1        PIC X.
               03 S025-CONTROL2.
                  05 S025-CONTROL2B    PIC X.
                  05 S025-CONTROL2B    PIC X(06).
JAPI=A     02  S025-DTEVENTO           PIC 9(09)    COMP-3.
           02  S025-CODLANC            PIC 9(03)    COMP-3.
JAPI=A     02  S025-DTCONTAB           PIC 9(09)    COMP-3.
JAPI=A     02  S025-DTACERT            PIC 9(09)    COMP-3.
           02  S025-IDENTIF            PIC X.
           02  S025-DEBCRED            PIC 9.
           02  S025-VLPRIORI           PIC S9(13)V99 COMP-3.
           02  S025-VLPRICOR           PIC S9(13)V99 COMP-3.
           02  S025-VLMUTORI           PIC S9(13)V99 COMP-3.
           02  S025-VLMUTCOR           PIC S9(13)V99 COMP-3.
           02  S025-VLPROORI           PIC 9(13)V99 COMP-3.
           02  S025-VLPROCOR           PIC 9(13)V99 COMP-3.
           02  S025-VLPRMUORI          PIC 9(13)V99 COMP-3.
           02  S025-VLPRMUCOR          PIC 9(13)V99 COMP-3.
           02  S025-VLAUXIL            PIC S9(13)V99 COMP-3.
                                       PIC 9(11)V9(04) COMP-3.
           02  S025-JURORIG            PIC 9(13)V99 COMP-3.
           02  S025-JURCORRI           PIC 9(13)V99 COMP-3.
           02  S025-CORRORIG           PIC 9(13)V99 COMP-3.
           02  S025-CORRCORRI          PIC 9(13)V99 COMP-3.
           02  S025-PROAORIG           PIC 9(13)V99 COMP-3.
           02  S025-PROACORR           PIC 9(13)V99 COMP-3.
           02  S025-ASSIORIG           PIC 9(13)V99 COMP-3.
           02  S025-ASSICORR           PIC 9(13)V99 COMP-3.
           02  S025-LAUDORIG           PIC 9(13)V99 COMP-3.
           02  S025-LAUDCORR           PIC 9(13)V99 COMP-3.
           02  S025-MEDIORIG           PIC 9(13)V99 COMP-3.
           02  S025-MEDICORR           PIC 9(13)V99 COMP-3.
           02  S025-REMUORIG           PIC 9(13)V99 COMP-3.
           02  S025-REMUCORR           PIC 9(13)V99 COMP-3.
           02  S025-IOFORIG            PIC 9(13)V99 COMP-3.
           02  S025-IOFCORR            PIC 9(13)V99 COMP-3.
           02  S025-AGDEBCRED          PIC 9(05)    COMP-3.
           02  S025-RZDEBCRED          PIC 9(05)    COMP-3.
           02  S025-CTDEBCRED          PIC 9(07)    COMP-3.
JAPI=A     02  S025-DTMORA             PIC 9(09)    COMP-3.
           02  S025-DBCRMORA           PIC 9.
           02  S025-NUMMORA            PIC 9(07)    COMP-3.
           02  S025-CODCRS             PIC 9(04).
JAPI=A     02  S025-DTACERTO           PIC 9(09)    COMP-3.
           02  S025-CODACERT           PIC 9(03)    COMP-3.
           02  FILLER                  PIC X(744).

       01  SSA04-CAD-NQ.
           03  FILLER                  PIC X(09) VALUE 'RURA025'.

      ********   AREAS DE TRABALHO PARA O CALCULO DO DOLAR.  ********

       01  FILLER.
           03  WRK-VLR-DOLAR         PIC 9(09)V9(04) VALUE ZEROS.
           03  WRK-MEDIORIG          PIC 9(13)V99    VALUE ZEROS.
           03  WRK-VLPRICOR          PIC 9(13)V99    VALUE ZEROS.
           03  WRK-VLAUXIL           PIC 9(13)V99    VALUE ZEROS.
           03  WRK-VLAUXIL-R REDEFINES WRK-VLAUXIL
                                     PIC 9(11)V9(04).



       01  DB-MEAG.
           03  MEAG-CHAVE              PIC 9(5)  COMP-3.
           03  MEAG-NOMEAG             PIC X(20).
           03  FILLER                  PIC X(60).
           03  MEAG-CODIGO             PIC 9(5)  COMP-3.
           03  FILLER                  PIC X(164).


JAPI=A********   AREA  DE  PASSAR  CONTROLE  PARA  RUCA6632   ********

JAPI=A 01  START-AREA-RUCA6632.
BS0214     03  WRK-START-LL6632        PIC 9(4)  COMP  VALUE 71.
           03  START-ZZ6632            PIC 9(4)  COMP  VALUE ZEROS.
           03  START-TRANCODE6632      PIC X(15)       VALUE 'RUCA6630'.
           03  START-SENHAS6632        PIC X(37).
           03  START-NOPCAO6632        PIC 9(02)       VALUE ZEROS.
BS0214     03  WRK-START-CHAVE6632     PIC 9(09)       VALUE ZEROS.
           03  START-ORDEM6632         PIC 9(03).
           03  START-IND6632           PIC X(01).


CET    01  WRK-CTR-MSG-6680.
CET        03  WRK-CTR-LL-6680         PIC 9(04)  COMP  VALUE  71.
CET        03  WRK-CTR-ZZ-6680         PIC 9(04)  COMP  VALUE   0.
CET        03  WRK-CTR-TRANSACAO-6680  PIC X(08)  VALUE SPACES.
CET        03  FILLER                  PIC X(07)     VALUE SPACES.
CET        03  WRK-CTR-SENHAS-6680     PIC X(37)     VALUE SPACES.
CET        03  WRK-CTR-NOPCAO-6680     PIC X(02)     VALUE SPACES.
CET        03  WRK-CTR-CPOS-6680.
CET            05  WRK-CTR-CHAVE-6680  PIC 9(09)     VALUE ZEROS.
CET            05  WRK-CTR-ORDEM-6680  PIC 9(03)     VALUE ZEROS.
CET            05  WRK-CTR-INDICA-6680.
CET                07  WRK-CTR-INDICA-6680-N PIC 9(01) VALUE ZEROS.

BR1020*---------------------------------------------------------------*
BR1020 01  FILLER                      PIC X(32)           VALUE
BR1020     '*** COMU-AREA COM RUCA6682   ***'.
BR1020*---------------------------------------------------------------*
BR1020
BR1020 01  WRK-CTR-MSG-6682.
BR1020     03 WRK-CTR-LL-6682          PIC 9(04)  COMP     VALUE ZEROS.
BR1020     03 WRK-CTR-TRANSACAO-6682   PIC X(08)           VALUE SPACES.
BR1020     03 FILLER                   PIC X(07)           VALUE SPACES.
BR1020     03 WRK-CTR-SENHAS-6682      PIC X(37)           VALUE SPACES.
BR1020     03 FILLER                   PIC X(02)           VALUE SPACES.
BR1020     03 WRK-CTR-CCONTR-6682.
BR1020        05 WRK-CTR-CCONTR-N-6682 PIC 9(009)          VALUE ZEROS.
BR1020     03 FILLER                   PIC X(004)          VALUE SPACES.
BR1020     03 WRK-CTR-CDAGEN-6682      PIC X(005)          VALUE SPACES.
BR1020     03 WRK-CTR-DSAGEN-6682      PIC X(030)          VALUE SPACES.
BR1020     03 WRK-CTR-CEDULA-6682      PIC X(007)          VALUE SPACES.
BR1020     03 WRK-CTR-NOMECL-6682      PIC X(031)          VALUE SPACES.
BR1020     03 WRK-CTR-CTACOR-6682      PIC X(009)          VALUE SPACES.
BR1020     03 WRK-CTR-CNPJCP-6682      PIC X(017)          VALUE SPACES.
BR1020
      ********   TELA  DE  ERRO   ********

WIP001*----------------------------------------------------------------*
WIP001 01  FILLER                      PIC  X(050)         VALUE
WIP001     '*** AREA DE COMUNICACAO - RUCA6650 ***'.
WIP001*----------------------------------------------------------------*
WIP001
WIP001     05  WRK-COMU-LL             PIC S9(004) COMP    VALUE +157.
WIP001     05  WRK-COMU-ZZ             PIC  9(004) COMP    VALUE ZEROS.
WIP001     05  WRK-COMU-TRANCODE       PIC  X(015)         VALUE SPACES.
WIP001     05  WRK-COMU-SENHA          PIC  X(037)         VALUE SPACES.
WIP001     05  WRK-COMU-NOPCAO         PIC  X(002)         VALUE SPACES.
WIP001     05  WRK-COMU-CHNUM          PIC  X(009)         VALUE SPACES.
WIP001     05  WRK-COMU-CHNUM-N        REDEFINES
WIP001         WRK-COMU-CHNUM          PIC  9(009).
WIP001     05  WRK-COMU-CHDIG          PIC  X(001)         VALUE SPACES.
WIP001     05  WRK-COMU-AGENCIA        PIC  X(005)         VALUE SPACES.
WIP001     05  WRK-COMU-NOMAG          PIC  X(020)         VALUE SPACES.
WIP001     05  WRK-COMU-AAFINAN        PIC  X(008)         VALUE SPACES.
WIP001     05  WRK-COMU-NOME           PIC  X(030)         VALUE SPACES.
WIP001     05  WRK-COMU-CC             PIC  X(009)         VALUE SPACES.
WIP001     05  WRK-COMU-CNPJCPF        PIC  X(017)         VALUE SPACES.

RUSSO1*----------------------------------------------------------------*
RUSSO1 01  FILLER                      PIC  X(050)         VALUE
RUSSO1     '*** AREA DE COMUNICACAO - RUCA6690 ***'.
RUSSO1*----------------------------------------------------------------*
RUSSO1
RUSSO1     05  WRK-COMU-LL-6690        PIC S9(004) COMP    VALUE +67.
RUSSO1     05  WRK-COMU-ZZ-6690        PIC  9(004) COMP    VALUE ZEROS.
RUSSO1     05  WRK-COMU-TRANCODE-6690  PIC  X(015)         VALUE SPACES.
RUSSO1     05  WRK-COMU-SENHA-6690     PIC  X(037)         VALUE SPACES.
RUSSO1     05  WRK-COMU-NOPCAO-6690    PIC  X(002)         VALUE SPACES.
RUSSO1     05  WRK-COMU-NROPER-6690    PIC  X(009)         VALUE SPACES.

STE161*----------------------------------------------------------------*
STE161 01  FILLER                      PIC  X(050)         VALUE
STE161     '*** AREA DE COMUNICACAO - RUCA6655 ***'.
STE161*----------------------------------------------------------------*
STE161
STE161 01  WRK-COMU-AREA-6655.
STE161     05  WRK-COMU-LL-6655        PIC S9(04) COMP    VALUE +74.
STE161     05  WRK-COMU-ZZ-6655        PIC  9(04) COMP    VALUE ZEROS.
STE161     05  WRK-COMU-TRANCODE-6655  PIC  X(08)         VALUE SPACES.
STE161     05  FILLER                  PIC  X(07)         VALUE SPACES.
STE161     05  WRK-COMU-SENHA-6655     PIC  X(37)         VALUE SPACES.
STE161     05  FILLER                  PIC  X(09)         VALUE SPACES.
STE161     05  WRK-COMU-NROPER-6655    PIC  X(09)         VALUE SPACES.

7C2603 01  FILLER                      PIC  X(050)         VALUE
7C2603     '*** AREA DE COMUNICACAO - RUCA7951 ***'.
7C2603*----------------------------------------------------------------*
7C2603
7C2603 01  WRK-COMU-AREA-7951.
7C2603     05  WRK-COMU-LL-7951        PIC S9(04) COMP    VALUE +74.
7C2603     05  WRK-COMU-ZZ-7951        PIC  9(04) COMP    VALUE ZEROS.
7C2603     05  WRK-COMU-TRANCODE-7951  PIC  X(08)         VALUE SPACES.
7C2603     05  FILLER                  PIC  X(006)       VALUE SPACES.
7C2603     05  WRK-COMU-PFK-7951       PIC  X(001)       VALUE SPACES.
7C2603     05  WRK-COMU-SENHAS-7951    PIC  X(037)       VALUE SPACES.
7C2603     05  WRK-COMU-OPCAO-7951     PIC  X(002)       VALUE SPACES.
7C2603     05  WRK-COMU-NROPER-7951.
7C2603         10 WRK-NROPER-N-7951    PIC  9(009)       VALUE ZEROS.
7C2603     05  WRK-COMU-AG-7951.
7C2603         10 WRK-AG-N-7951        PIC  9(005)       VALUE ZEROS.
7C2603     05  WRK-COMU-CC-7951        PIC  X(09)        VALUE SPACES.
7C2603     05  WRK-COMU-CGCCPF-7951    PIC  X(17)        VALUE SPACES.

      *---------------------------------------------------------------*
      *   PARAMETRO A SER PASSADO P/ A POOL7100 (MODULO TRATA/ ERRO)  *
      *---------------------------------------------------------------*
       01  ERRO-AREA.
           05  ERR-TIPO-ACESSO        PIC X(03).
           05  ERR-PGM                PIC X(08).
           05  ERR-DADOS-IMS-DB2.
               07  ERR-DBD-TAB        PIC X(18).
               07  ERR-FUN-COMANDO    PIC X(10).
               07  ERR-STA-CODE       PIC X(04).
               07  ERR-SQL-CODE  REDEFINES
                   ERR-STA-CODE       PIC S9(09) COMP-4.
               07  ERR-LOCAL          PIC X(04).
               07  ERR-SEGM           PIC X(08).
               07  FILLER             PIC X(31).

           05  ERR-TEXTO  REDEFINES  ERR-DADOS-IMS-DB2
                                      PIC X(75).
      *---------------------------------------------------------------*
      *  OS CPOS ABAIXO DEVEM SER INICIALIZADOS SO' QDO O APLICATIVO  *
      *  USAR O SISTEMA DE SENHAS (POOL1000).                         *
      *---------------------------------------------------------------*
           05  ERR-DADOS-SENHAS.
               07  ERR-COD-DEPTO      PIC X(06).
               07  ERR-MODULO         PIC X(08)  VALUE SPACES.


      ********   MENSAGEM  DE  ERRO   ********

       01  MENS-ERR.
           03  FILLER                  PIC X(44)        VALUE '*** ERRO
      -        'NO ACESSO A POOL0660 - RETURN-CODE '.
           03  ERR-RC                  PIC 99.
           03  FILLER                  PIC X(7)         VALUE ' *** - '.
           03  ERR-TELA                PIC X(8).
           03  FILLER                  PIC X(10)     VALUE ' - LOCAL  '.
           03  ERR-LOC                 PIC 99.


      ********   AREAS  PARA  PESQUISA   ********

       01  SSA1-CAD.
           03  SSA-NOME-SEG-CAD        PIC X(8)        VALUE 'RURA020'.
           03  FILLER                  PIC X           VALUE '('.
           03  FILLER                  PIC X(2)        VALUE '= '.
JAPI=A     03  SSA-CHAVE               PIC 9(9) COMP-3.
           03  FILLER                  PIC X           VALUE ')'.
NEW-->*
NEW--> 01      HR-SSA01  REDEFINES  SSA1-CAD                           .
NEW-->      02 FILLER                         PIC X(019)               .
NEW-->      02 HR-SSA011-C-CH-CONTR           PIC S9(09)V      COMP-3  .
NEW-->*

       01  SSA2-CAD.
           03  FILLER                  PIC X(9)        VALUE 'RURA020 '.

       01  SSA1-AG.
           03  SSA-NOME-SEG-AG         PIC X(8)        VALUE 'MEAG005'.
           03  FILLER                  PIC X           VALUE '('.
           03  FILLER                  PIC X(8)        VALUE 'CHV03AA'.
           03  FILLER                  PIC X(2)        VALUE '= '.
           03  SSA-AG                  PIC 9(5) COMP-3 VALUE ZEROS.
           03  FILLER                  PIC X           VALUE ')'.
NEW-->*
NEW--> 01      HR-SSA03  REDEFINES  SSA1-AG                            .
NEW-->      02 HR-SSA031-CDAGENC              PIC  9(05)       COMP-3  .
NEW-->*
NEW--> 77  HR-PERFORM-STATUS           PIC X(001).
NEW-->     88  HR-PREVIOUS-OK                     VALUE 'Y'.
NEW--> 77  HR-PROGRAM-ID               PIC X(008) VALUE 'RURA6300'.
NEW--> 77  HR-CALL-NUM                 PIC X(003).
NEW--> 77  HR-CALL-ID                  PIC X(001).
NEW--> 77  HR-CURRENT-SQLSTMT          PIC X(006).
NEW--> 77  HR-PRIMARY-SQLCODE          PIC S9(09) COMP.
NEW--> 77  HR-EDITED-SQLCODE           PIC +9(03).
NEW--> 77  HR-RETURN-CODE              PIC S9(04) COMP.
NEW-->*
NEW--> 01  HR-PCB-AREA.
NEW-->     03  HR-PCB-DBDNM            PIC X(008) VALUE '*HRDBD**'.
NEW-->     03  HR-PCB-LEVEL            PIC X(002) VALUE '00'.
NEW-->     03  HR-PCB-STATCD           PIC X(002) VALUE SPACES.
NEW-->     03  HR-PCB-PROCOPT          PIC X(004) VALUE '****'.
NEW-->     03  HR-PCB-RESERV           PIC S9(09) COMP VALUE ZEROS.
NEW-->     03  HR-PCB-SEGNM            PIC X(008) VALUE '*HRSEG**'.
NEW-->     03  HR-PCB-KEYLEN           PIC S9(09) COMP VALUE ZEROS.
NEW-->     03  HR-PCB-SENSEG           PIC S9(09) COMP VALUE ZEROS.

       01 FILLER.
           03 TABELA.
               05 FILLER               PIC X(41) VALUE
                  '0027CALCADA-SALVADOR     15.142.490000219'.
               05 FILLER               PIC X(41) VALUE
                  '0035CEASA-SALVADOR       15.142.490000308'.
               05 FILLER               PIC X(41) VALUE
                  '0060RIO DE JANEIRO       15.142.490000642'.
               05 FILLER               PIC X(41) VALUE
                  '0078ALAGOINHAS           15.142.490000723'.
               05 FILLER               PIC X(41) VALUE
                  '0086BARREIRAS            15.142.490000804'.
               05 FILLER               PIC X(41) VALUE
                  '0094BOM JESUS DA LAPA    15.142.490000995'.
               05 FILLER               PIC X(41) VALUE
                  '0108BREJOES              15.142.490001029'.
               05 FILLER               PIC X(41) VALUE
                  '0116CAMACA               15.142.490001100'.
               05 FILLER               PIC X(41) VALUE
                  '0132CANCEICAO DO ALMEIDA 15.142.490001371'.
                  '0140ESPLANADA            15.142.490001452'.
               05 FILLER               PIC X(41) VALUE
                  '0159EUCLIDES DA CUNHA    15.142.490001533'.
               05 FILLER               PIC X(41) VALUE
                  '0167FEIRA DE SANTANA     15.142.490001614'.
               05 FILLER               PIC X(41) VALUE
                  '0175GANDU                15.142.490001703'.
               05 FILLER               PIC X(41) VALUE
                  '0186IGUAI                15.142.490001886'.
               05 FILLER               PIC X(41) VALUE
                  '0191ILHEUS               15.142.490001967'.
               05 FILLER               PIC X(41) VALUE
                  '0205IPIAU                15.142.490002009'.
               05 FILLER               PIC X(41) VALUE
                  '0213ITABEABA             15.142.490002101'.
               05 FILLER               PIC X(41) VALUE
                  '0631ITABUNA              15.142.490002262'.
               05 FILLER               PIC X(41) VALUE
                  '0240ITIRUCU              15.142.490002424'.
               05 FILLER               PIC X(41) VALUE
                  '0256ITORORO              15.142.490002505'.
                  '0264JEQUIE               15.142.490002696'.
               05 FILLER               PIC X(41) VALUE
                  '0272JUAZEIRO             15.142.490002777'.
               05 FILLER               PIC X(41) VALUE
                  '0280LIVRAMENTO N. SENHORA15.142.490002850'.
               05 FILLER               PIC X(41) VALUE
                  '0310MEDEIROS NETO        15.142.490003153'.
               05 FILLER               PIC X(41) VALUE
                  '0329MORRO DO CHAPEU      15.142.490003234'.
               05 FILLER               PIC X(41) VALUE
                  '0337MURITIBA             15.142.490003316'.
               05 FILLER               PIC X(41) VALUE
                  '0345NAZARE               15.142.490003404'.
               05 FILLER               PIC X(41) VALUE
                  '0353PARAMIRIM            15.142.490003587'.
               05 FILLER               PIC X(41) VALUE
                  '0361PORTO SEGURO         15.142.490003668'.
               05 FILLER               PIC X(41) VALUE
                  '0370POTIRAGUA            15.142.490003749'.
               05 FILLER               PIC X(41) VALUE
                  '0389RUY BARBOSA          15.142.490003820'.
                  '0396STA MARIA VITORIA    15.142.490003900'.
               05 FILLER               PIC X(41) VALUE
                  '0400SANTANA              15.142.490004044'.
               05 FILLER               PIC X(41) VALUE
                  '0418SANTO ANTONIO JESUS  15.142.490004125'.
               05 FILLER               PIC X(41) VALUE
                  '0426SEABRA               15.142.490004206'.
               05 FILLER               PIC X(41) VALUE
                  '0434SENHOR DO BONFIM     15.142.490004397'.
               05 FILLER               PIC X(41) VALUE
                  '0442TUCANO               15.142.490004479'.
               05 FILLER               PIC X(41) VALUE
                  '0485VITORIA DA CONQUISTA 15.142.490004800'.
               05 FILLER               PIC X(41) VALUE
                  '0493XIQUE-XIQUE          15.142.490004982'.
               05 FILLER               PIC X(41) VALUE
                  '0507SAO PEDRO-SALVADOR   15.142.490005016'.
               05 FILLER               PIC X(41) VALUE
                  '0515SHOPING SBS          15.142.490005105'.
               05 FILLER               PIC X(41) VALUE
                  '0523ITAPETINGA           15.142.490005280'.
                  '0540GUANAMBI             15.142.490005440'.
               05 FILLER               PIC X(41) VALUE
                  '0574CAMPO GRANDE-SDOR    15.142.490005792'.
               05 FILLER               PIC X(41) VALUE
                  '0582CONCEICAO DO JACUIPE 15.142.490005873'.
               05 FILLER               PIC X(41) VALUE
                  '0590ROQUIRA              15.142.490005954'.
               05 FILLER               PIC X(41) VALUE
                  '0604JOCOBINA             15.142.490006098'.
               05 FILLER               PIC X(41) VALUE
                  '0612POJUCA               15.142.490006171'.
               05 FILLER               PIC X(41) VALUE
                  '0620SANTO ESTEVAO        15.142.490006250'.
               05 FILLER               PIC X(41) VALUE
                  '0639SAO FCO DO CONDE     15.142.490006330'.
               05 FILLER               PIC X(41) VALUE
                  '0647TEIXEIRA DE FREITAS  15.142.490006411'.
               05 FILLER               PIC X(41) VALUE
                  '0671CENTRO-SALVADOR      15.142.490006764'.
               05 FILLER               PIC X(41) VALUE
                  '0680CATU                 15.142.490006845'.
                  '0692URUCUCA              15.142.490006926'.
               05 FILLER               PIC X(41) VALUE
                  '0701JEREMOABO            15.142.490007060'.
               05 FILLER               PIC X(41) VALUE
                  '0710CAB-SALVADOR         15.142.490007140'.
               05 FILLER               PIC X(41) VALUE
                  '0728ENCRUZILHADA         15.142.490007631'.
               05 FILLER               PIC X(41) VALUE
                  '0752SANTA INES           15.142.490007574'.
               05 FILLER               PIC X(41) VALUE
                  '0760RIACHO DO JACUIPE    15.142.490007658'.
               05 FILLER               PIC X(41) VALUE
                  '0779FLORESTA AZUL        15.142.490007736'.
               05 FILLER               PIC X(41) VALUE
                  '0787RIO REAL             15.142.490007817'.
               05 FILLER               PIC X(41) VALUE
                  '0795CAMACARI             15.142.490007906'.
               05 FILLER               PIC X(41) VALUE
                  '0809CONDEUBA             15.142.490008031'.
               05 FILLER               PIC X(41) VALUE
                  '0817COTEGIPE             15.142.490008112'.
                  '0826MUCURI               15.142.490008201'.
               05 FILLER               PIC X(41) VALUE
                  '0833BROTAS DE MACAUBA    15.142.490008394'.
               05 FILLER               PIC X(41) VALUE
                  '0841SOBRADINHO           15.142.490008465'.
               05 FILLER               PIC X(41) VALUE
                  '0868CORRENTINA           15.142.490008627'.
               05 FILLER               PIC X(41) VALUE
                  '0892TAPIRAMUTA           15.142.490008970'.
               05 FILLER               PIC X(41) VALUE
                  '0930IGUATEMI             15.142.490009356'.
               05 FILLER               PIC X(41) VALUE
                  '0949ITAJU DO COLONIA     15.142.490009437'.
               05 FILLER               PIC X(41) VALUE
                  '0957LAGEDAO              15.142.490009519'.
               05 FILLER               PIC X(41) VALUE
                  '0965DARIO MEIRA          15.142.490009607'.
               05 FILLER               PIC X(41) VALUE
                  '0973ITAGIMIRIM           15.142.490009780'.
               05 FILLER               PIC X(41) VALUE
                  '0981NILO PECANHA         15.142.490009860'.
                  '1031BARRA DO CHOCA       15.142.490010362'.
               05 FILLER               PIC X(41) VALUE
                  '1040BOM DESPACHO         15.142.490010443'.
               05 FILLER               PIC X(41) VALUE
                  '1074RIO DO PIRES         15.142.490010796'.
               05 FILLER               PIC X(41) VALUE
                  '1090SAUDE                15.142.490010958'.
               05 FILLER               PIC X(41) VALUE
                  '1155ABATE                15.142.490011504'.
               05 FILLER               PIC X(41) VALUE
                  '1163WAGNER               15.142.490011667'.
               05 FILLER               PIC X(41) VALUE
                  '1171VARZEA DO POCO       15.142.490011768'.
               05 FILLER               PIC X(41) VALUE
                  '1198RIBEIRA DO POMBAL    15.142.490011920'.
               05 FILLER               PIC X(41) VALUE
                  '1210MONTE SANTO          15.142.490012144'.
               05 FILLER               PIC X(41) VALUE
                  '1228PIRIPA               15.142.490016325'.
               05 FILLER               PIC X(41) VALUE
                  '1244ITUACU               15.142.490012497'.
                  '1287PRES.JANIO QUADROS   15.142.490012810'.
               05 FILLER               PIC X(41) VALUE
                  '1295ARACATU              15.142.490012900'.
               05 FILLER               PIC X(41) VALUE
                  '1325TANQUE NOVO          15.142.490013206'.
               05 FILLER               PIC X(41) VALUE
                  '1333BREJOLANDIA          15.142.490013380'.
               05 FILLER               PIC X(41) VALUE
                  '1341PIATA                15.142.490013469'.
               05 FILLER               PIC X(41) VALUE
                  '1350IRECE                15.142.490013540'.
               05 FILLER               PIC X(41) VALUE
                  '1376AMARGOSA             15.142.490013701'.
               05 FILLER               PIC X(41) VALUE
                  '1406CRISTOPOLIS          15.142.490014007'.
               05 FILLER               PIC X(41) VALUE
                  '1414MAIQUINIQUE          15.142.490014199'.
               05 FILLER               PIC X(41) VALUE
                  '1422ITAMARAJU            15.142.490014279'.
               05 FILLER               PIC X(41) VALUE
                  '1430MACURURE             15.142.490014350'.
                  '1473BRUMADO              15.142.490014783'.
               05 FILLER               PIC X(41) VALUE
                  '1481VALENCA              15.142.490014864'.
               05 FILLER               PIC X(41) VALUE
                  '1503BARRA-SALVADOR       15.142.490015089'.
               05 FILLER               PIC X(41) VALUE
                  '1520IPUPIARA             15.142.490015240'.
               05 FILLER               PIC X(41) VALUE
                  '1538CONTENDAS SINCORA    15.142.490015321'.
               05 FILLER               PIC X(41) VALUE
                  '1554SOUTO SOARES         15.142.490015593'.
               05 FILLER               PIC X(41) VALUE
                  '1562JAGUARARI            15.142.490015674'.
               05 FILLER               PIC X(41) VALUE
                  '1589CACULE               15.142.490015836'.
               05 FILLER               PIC X(41) VALUE
                  '1597CORDEIROS            15.142.490015917'.
               05 FILLER               PIC X(41) VALUE
                  '1600RODELAS              15.142.490016050'.
               05 FILLER               PIC X(41) VALUE
                  '1619PLANALTINO           15.142.490016131'.
                  '1635CAETITE              15.142.490016201'.
               05 FILLER               PIC X(41) VALUE
                  '1651LAMARAO              15.142.490016565'.
               05 FILLER               PIC X(41) VALUE
                  '1686TEOLANDIA            15.142.490016808'.
               05 FILLER               PIC X(41) VALUE
                  '1708TABOCAS DO B. VELHO  15.142.490017022'.
               05 FILLER               PIC X(41) VALUE
                  '1716CORIBE               15.142.490017103'.
               05 FILLER               PIC X(41) VALUE
                  '1724PEDRO ALEXANDRE      15.142.490017284'.
               05 FILLER               PIC X(41) VALUE
                  '1732PITUBA               15.142.490017375'.
               05 FILLER               PIC X(41) VALUE
                  '1759NOVA VICOSA          15.142.490017537'.
               05 FILLER               PIC X(41) VALUE
                  '1767UBAITABA             15.142.490017618'.
               05 FILLER               PIC X(41) VALUE
                  '1775SERRA PRETA          15.142.490017707'.
               05 FILLER               PIC X(41) VALUE
                  '1791BROTAS DE MACAUBA    15.142.490017960'.
                  '1821PEDRAO               15.142.490018266'.
               05 FILLER               PIC X(41) VALUE
                  '1856ITABELA              15.142.490018509'.
               05 FILLER               PIC X(41) VALUE
                  '1872MORPARA              15.142.490018770'.
               05 FILLER               PIC X(41) VALUE
                  '1880PAULO AFONSO         15.142.490018851'.
               05 FILLER               PIC X(41) VALUE
                  '1899IPECAETA             15.142.490018932'.
               05 FILLER               PIC X(41) VALUE
                  '1945PALMAS DO M. ALTO    15.142.490019408'.
               05 FILLER               PIC X(41) VALUE
                  '1953CAMPO FORMOSO        15.142.490019580'.
               05 FILLER               PIC X(41) VALUE
                  '1970COARACI              15.142.490019742'.
               05 FILLER               PIC X(41) VALUE
                  '1988ICHU                 15.142.490019823'.
               05 FILLER               PIC X(41) VALUE
                  '1996CAATIBA              15.142.490019904'.
               05 FILLER               PIC X(41) VALUE
                  '2003IBICOARA             15.142.490020082'.
                  '2011IBIPEBA              15.142.490020163'.
               05 FILLER               PIC X(41) VALUE
                  '2062JANDAIRA             15.142.490020678'.
               05 FILLER               PIC X(41) VALUE
                  '2070SIMOES FILHO         15.142.490020759'.
               05 FILLER               PIC X(41) VALUE
                  '2100SANTA BRIGIDA        15.142.490021054'.
               05 FILLER               PIC X(41) VALUE
                  '2119ANTONIO GONCALVES    15.142.490021135'.
               05 FILLER               PIC X(41) VALUE
                  '2127IBIPITANGA           15.142.490021216'.
               05 FILLER               PIC X(41) VALUE
                  '2143CONCEICAO DO COITE   15.142.490021488'.
               05 FILLER               PIC X(41) VALUE
                  '2161QUIJINQUE            15.142.490021669'.
               05 FILLER               PIC X(41) VALUE
                  '2178STA CRUZ DA VITORIA  15.142.490021720'.
               05 FILLER               PIC X(41) VALUE
                  '6324CRISOPOLIS           15.142.490063298'.
               05 FILLER               PIC X(41) VALUE
                  '2232IRARA                15.142.490022379'.
                  '2240OURICANGAS           15.142.490022450'.
               05 FILLER               PIC X(41) VALUE
                  '2259CANAVIEIRAS          15.142.490022630'.
               05 FILLER               PIC X(41) VALUE
                  '2267PORTO SECO PIRAJA    15.142.490022611'.
               05 FILLER               PIC X(41) VALUE
                  '2283MALHADA DE PEDRAS    15.142.490022883'.
               05 FILLER               PIC X(41) VALUE
                  '2299ITAJUIPE             15.142.490022964'.
               05 FILLER               PIC X(41) VALUE
                  '2305CHORROCHO            15.142.490023006'.
               05 FILLER               PIC X(41) VALUE
                  '2313REMANSO              15.142.490023189'.
               05 FILLER               PIC X(41) VALUE
                  '2330IPIRA                15.142.490023340'.
               05 FILLER               PIC X(41) VALUE
                  '2356GRACA                15.142.490023502'.
               05 FILLER               PIC X(41) VALUE
                  '2364EUNAPOLIS            15.142.490023693'.
               05 FILLER               PIC X(41) VALUE
                  '2372CRUZ DAS ALMAS       15.142.490023774'.
                  '2380SANTO AMARO          15.142.490023855'.
               05 FILLER               PIC X(41) VALUE
                  '2399SERRINHA             15.142.490023936'.
               05 FILLER               PIC X(41) VALUE
                  '2402BAIRRO BRASIL        15.142.490024070'.
               05 FILLER               PIC X(41) VALUE
                  '2410IBOTIRAMA            15.142.490024150'.
               05 FILLER               PIC X(41) VALUE
                  '2429BASE                 15.142.490024231'.
               05 FILLER               PIC X(41) VALUE
                  '2437SERVIDOR             15.142.490024312'.
               05 FILLER               PIC X(41) VALUE
                  '2445SENHOR DOS PASSOS    15.142.490024401'.
               05 FILLER               PIC X(41) VALUE
                  '2453PELOURINHO           15.142.490024504'.
               05 FILLER               PIC X(41) VALUE
                  '2461SUMARE               15.142.490024665'.
               05 FILLER               PIC X(41) VALUE
                  '2470PERIPERI             15.142.490024746'.
               05 FILLER               PIC X(41) VALUE
                  '2488ITAPUA               15.142.490024827'.
                  '2496LITORAL NORTE        15.142.490024908'.
               05 FILLER               PIC X(41) VALUE
                  '2500LUIZ E. MAGALHAES    15.142.490025041'.
               05 FILLER               PIC X(41) VALUE
                  '2518RIO VERMELHO         15.142.490025122'.
               05 FILLER               PIC X(41) VALUE
                  '2526SHOPPING BARRA       15.142.490025203'.
               05 FILLER               PIC X(41) VALUE
                  '2534UNIQ                 15.142.490025394'.
               05 FILLER               PIC X(41) VALUE
                  '2542FAZENDA GRANDE       15.142.490025475'.
           03 TAB-INTERNA REDEFINES  TABELA
                          OCCURS 169 TIMES INDEXED BY WRK-IND.
               05 WRK-TB-COD       PIC 9(03).
               05 WRK-TB-DIG       PIC 9(01).
               05 WRK-TB-NOME      PIC X(21).
               05 WRK-TB-CGC       PIC 9(10).
               05 WRK-TB-FILIAL    PIC 9(04).
               05 WRK-TB-CTL       PIC 9(02).

NEW-->*
NEW-->*
NEW-->     EXEC SQL INCLUDE MESUV001 END-EXEC.
NEW-->     EXEC SQL INCLUDE RUCAB003 END-EXEC.
NEW-->     EXEC SQL INCLUDE RUCAB004 END-EXEC.
VERI-I     EXEC SQL INCLUDE RUFIV002 END-EXEC.
BR0118     EXEC SQL INCLUDE RURCB00D END-EXEC.
7C0219     EXEC SQL INCLUDE RUCAB011 END-EXEC.
RO0419     EXEC SQL INCLUDE CMRUB001 END-EXEC.
RO0419     EXEC SQL INCLUDE RURCV040 END-EXEC.
RO0419     EXEC SQL INCLUDE RURCV037 END-EXEC.
NEW-->*
NEW-->*/*  EXEC SQL END DECLARE SECTION END-EXEC.
NEW-->*
NEW--> 77      HR-CURSOR-RUCAB004-003-04      PIC X(006) VALUE 'CLOSED'.
NEW--> 77      HR-003-C-CH-CONTR              PIC S9(09)V      COMP-3  .
NEW--> 77      HR-003-D-CH-NUMERO             PIC S9(07)V      COMP-3  .
NEW--> 77      HR-003-D-CH-TIPO               PIC S9(03)V      COMP-3  .
NEW--> 77      HR-004-CDAGENC                 PIC  9(05)       COMP-3  .
NEW-->*
NEW-->*/*  EXEC SQL BEGIN DECLARE SECTION END-EXEC.
NEW-->*
NEW--> 77      HR-NULL-C-CATEGMUT             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-CENTRO               PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-CLASPRODT            PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-CODEMBAL             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-CTAVINC              PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-DATA1                PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-DATA2                PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-DATA3                PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-DATA4                PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-DTACERTO             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-DTREFINAM            PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-EGFOUTROS            PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-EMPREEND             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-FIANCA               PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-FINALID              PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-FISCALIZ             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-GARANTIA             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-MUNIC                PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-NUMAUTOR             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-ORIGREC1             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-PRODUTOR             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-SAFRA-INI            PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-TITULO               PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-UNIDADE1             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-UNIDADE2             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-C-VFINAL               PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-D-DTACERT              PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-D-DTACERTO             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-D-DTCONTAB             PIC S9(04)       COMP    .
NEW--> 77      HR-NULL-D-DTEVENTO             PIC S9(04)       COMP    .
BR0118 77      WRK-NULL-C-DVALDD-DECLR        PIC S9(04)       COMP
BR0118                                                      VALUE ZEROS.
BR0118 77      WRK-NULL-C-DECLR-AGCLT         PIC S9(04)       COMP
BR0118                                                      VALUE ZEROS.
BR0118 77      WRK-NULL-C-CGRP-AGCLT          PIC S9(04)       COMP
BR0118                                                      VALUE ZEROS.
RO0419 77      WRK-NULL-C-CDBARRAS            PIC S9(04)       COMP
RO0419                                                      VALUE ZEROS.
ECR    77      WRK-NULL-CFORMA-LIQDC          PIC S9(04)       COMP
RO0419                                                      VALUE ZEROS.
NEW-->*
NEW-->*/*  EXEC SQL END DECLARE SECTION END-EXEC.
NEW-->     EXEC SQL INCLUDE SQLCA END-EXEC.
NEW-->*
NEW--> 01  CAD-PCB.
NEW-->     03  CAD-TERNAME             PIC X(8).
NEW-->     03  FILLER                  PIC X(2).
NEW-->     03  CAD-STATUS              PIC X(2).
NEW-->     03  FILLER                  PIC X(8).
NEW-->     03  CAD-SEGNAME             PIC X(8).
NEW-->*
NEW--> 01  MEAG-PCB.
NEW-->     03  MEAG-TERNAME            PIC X(8).
NEW-->     03  FILLER                  PIC X(2).
NEW-->     03  MEAG-STATUS             PIC X(2).
NEW-->     03  FILLER                  PIC X(8).
NEW-->     03  MEAG-SEGNAME            PIC X(8).
NEW-->*
NEW--> 01      HRMEAG05                                                .
NEW-->      02 HRMEAG05-CDAGENC               PIC  9(05)       COMP-3  .
NEW-->      02 HRMEAG05-NOMEAGE               PIC X(020)               .
NEW-->      02 FILLER                         PIC X(060)               .
NEW-->      02 HRMEAG05-MEAGCOD               PIC  9(05)       COMP-3  .
NEW-->*
NEW--> 01      HR03CAD1                                                .
POL128****  02 FILLER                         PIC X(003)               .
POL128      02 HR03CAD1-C-SEGURADORA          PIC S9(03)V      COMP-3  .
POL128      02 FILLER                         PIC X(001)               .
NEW-->      02 HR03CAD1-C-CH-CONTR            PIC S9(09)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(018)               .
NEW-->      02 HR03CAD1-C-CTACORR             PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD1-C-CTAVINC             PIC S9(07)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(049)               .
NEW-->      02 HR03CAD1-C-PRODUTOR            PIC  9(02)               .
NEW-->      02 FILLER                         PIC X(098)               .
NEW-->      02 HR03CAD1-C-AGENCIA             PIC S9(05)V      COMP-3  .
NEW-->      02 HR03CAD1-C-NUMEMPR             PIC S9(09)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(006)               .
NEW-->      02 HR03CAD1-C-MUNIC               PIC S9(13)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(002)               .
NEW-->      02 HR03CAD1-C-CENTRO              PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(019)               .
NEW-->      02 HR03CAD1-C-AREATOT             PIC S9(13)V9(02) COMP-3  .
NEW-->      02 FILLER                         PIC X(001)               .
NEW-->      02 HR03CAD1-C-FIANCA              PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(057)               .
NEW-->      02 HR03CAD1-C-TITULO              PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD1-C-GARANTIA            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD1-C-QTDBENS             PIC S9(15)V      COMP-3  .
NEW-->      02 HR03CAD1-C-VLBENS              PIC S9(13)V9(02) COMP-3  .
NEW-->      02 FILLER                         PIC X(020)               .
VERI-I      02 FILLER                         PIC X(003)               .
NEW-->      02 FILLER                         PIC X(029)               .
NEW-->      02 HR03CAD1-C-NUMRECOR            PIC S9(09)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(016)               .
NEW-->      02 HR03CAD1-C-NUMAUTOR            PIC S9(15)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(001)               .
NEW-->      02 HR03CAD1-C-FISCALIZ            PIC  9(02)               .
NEW-->      02 HR03CAD1-C-ATIVID              PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(002)               .
NEW-->      02 HR03CAD1-C-FINALID             PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD1-C-EMPREEND            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD1-C-QTDFINAN            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD1-C-UNIDADE1            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD1-C-PRODPROV            PIC S9(15)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(002)               .
NEW-->      02 HR03CAD1-C-SAFRA-INI           PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD1-C-SAFRA-FIM           PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD1-C-TOTFINAN            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD1-C-RECPROPR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 FILLER                         PIC X(010)               .
NEW-->      02 HR03CAD1-C-CODINCID            PIC  9(02)               .
NEW-->      02 HR03CAD1-C-DTVENCTO            PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD1-C-DTCEDULA            PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD1-C-DTOPER              PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD1-C-DTREGIST            PIC S9(09)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(016)               .
NEW-->      02 HR03CAD1-C-CLASPRODT           PIC S9(05)V      COMP-3  .
NEW-->      02 HR03CAD1-C-CODEMBAL            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD1-C-CATEGMUT            PIC  9(03)               .
NEW-->      02 HR03CAD1-C-CAPPRODUC           PIC S9(11)V      COMP-3  .
NEW-->      02 HR03CAD1-C-EGFOUTROS           PIC S9(11)V      COMP-3  .
POL128      02 FILLER                         PIC X(436)               .
POL128      02 XR03CAD1-C-SEGURADORA          PIC S9(03)V      COMP-3  .
POL128      02 FILLER                         PIC X(030)               .

NEW-->      02 FILLER                         PIC X(010)               .
NEW-->      02 HR03CAD2-C-CONTROL             PIC X(016)               .
NEW-->      02 FILLER                         PIC X(008)               .
NEW-->      02 HR03CAD2-C-NOMUTUAR1           PIC X(040)               .
NEW-->      02 HR03CAD2-C-PESSOA1             PIC X(001)               .
NEW-->      02 HR03CAD2-C-CGCPF1              PIC S9(15)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(002)               .
NEW-->      02 HR03CAD2-C-NOMUTUAR2           PIC X(040)               .
NEW-->      02 HR03CAD2-C-PESSOA2             PIC X(001)               .
NEW-->      02 HR03CAD2-C-CGCPF2              PIC S9(15)V      COMP-3  .
NEW-->      02 HR03CAD2-C-NOMUTUAR3           PIC X(040)               .
NEW-->      02 HR03CAD2-C-PESSOA3             PIC X(001)               .
NEW-->      02 HR03CAD2-C-CGCPF3              PIC S9(15)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(008)               .
NEW-->      02 HR03CAD2-C-NUMDEPEN            PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD2-C-TPDEPEN             PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(007)               .
NEW-->      02 HR03CAD2-C-MICREGIAO           PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(002)               .
NEW-->      02 HR03CAD2-C-UF                  PIC X(002)               .
NEW-->      02 HR03CAD2-C-REGIAO              PIC X(001)               .
NEW-->      02 HR03CAD2-C-ANOPROD             PIC S9(05)V      COMP-3  .
NEW-->      02 HR03CAD2-C-VLPROD              PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-QTDMVR              PIC S9(07)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(008)               .
NEW-->      02 HR03CAD2-C-INDENIZ             PIC X(001)               .
NEW-->      02 FILLER                         PIC X(042)               .
NEW-->      02 HR03CAD2-C-AGENAVAL            PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD2-C-CTAAVAL             PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD2-C-NOMAVAL             PIC X(040)               .
NEW-->      02 HR03CAD2-C-PESAVAL             PIC X(001)               .
NEW-->      02 HR03CAD2-C-CGCPFAVAL           PIC S9(15)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(020)               .
NEW-->      02 HR03CAD2-C-ESPBENS             PIC X(020)               .
VERI-I      02 HR03CAD2-C-ORIGREC1            PIC S9(03)V      COMP-3  .
VERI-I      02 HR03CAD2-C-IDENT1              PIC X(001)               .
NEW-->      02 HR03CAD2-C-DATA1               PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD2-C-ORIGREC2            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-IDENT2              PIC X(001)               .
NEW-->      02 HR03CAD2-C-DATA2               PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD2-C-ORIGREC3            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-IDENT3              PIC X(001)               .
NEW-->      02 HR03CAD2-C-ORIGREC4            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-IDENT4              PIC X(001)               .
NEW-->      02 HR03CAD2-C-DATA4               PIC S9(09)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(005)               .
NEW-->      02 HR03CAD2-C-IDENTREC            PIC X(001)               .
NEW-->      02 HR03CAD2-C-PLREFINAM           PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-SPRGREFIN           PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-DTREFINAM           PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD2-C-TXREFIN             PIC S9(05)V      COMP-3  .
NEW-->      02 HR03CAD2-C-NCARTREF            PIC S9(05)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(008)               .
NEW-->      02 HR03CAD2-C-POSTO               PIC X(001)               .
NEW-->      02 FILLER                         PIC X(004)               .
NEW-->      02 HR03CAD2-C-APLIC               PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(024)               .
NEW-->      02 HR03CAD2-C-FXPRODUT            PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(024)               .
NEW-->      02 HR03CAD2-C-VLBASCUST           PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-LIMFINAN            PIC S9(03)V      COMP-3  .
NEW-->      02 FILLER                         PIC X(022)               .
NEW-->      02 HR03CAD2-C-VLCOBPRO            PIC S9(13)V9(02) COMP-3  .
            02 FILLER                         PIC X(020)               .
NEW-->      02 HR03CAD2-C-PRECPRODT           PIC S9(15)V      COMP-3  .
NEW-->      02 HR03CAD2-C-PRECEMBAL           PIC S9(15)V      COMP-3  .
NEW-->      02 HR03CAD2-C-LIMPRODUC           PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-LIMPRODUT           PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-VFINAL              PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD2-C-JUROS               PIC S9(05)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-CORRECAO            PIC S9(05)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-ADPRAGRO            PIC S9(05)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-COBERTU             PIC S9(05)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-INCPAGRO            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLCOBERT            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-CODCRS              PIC X(004)               .
NEW-->      02 HR03CAD2-C-DTACERTO            PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD2-C-CODACERT            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD2-C-VLPRINCT-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPRINAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLMUTORI-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLMUTCOR-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPROCTA-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPROACE-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPRMUAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLAUXIL-OPER999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-JURPGORI-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-JURPGAC-PAG999      PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-CORRPGOR-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-CORRPGAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PROAPGOR-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PROAPGAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-TECNPGOR-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-TECNPGAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PERIPGOR-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PERIPGAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-MEDIPGOR-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-MEDIPGAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-REMUPGOR-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-REMUPGAC-PAG999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-IOFPGORI-CAP999     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-IOFPGAC-PAG999      PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPRINCT-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPRINAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLMUTORI-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPROCTA-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPROACE-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPRMUOR-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLPRMUAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-VLAUXIL-OPER998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-JURPGORI-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-JURPGAC-PAG998      PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-CORRPGOR-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-CORRPGAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PROAPGOR-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PROAPGAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-TECNPGOR-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-TECNPGAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PERIPGOR-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-PERIPGAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-MEDIPGOR-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-MEDIPGAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-REMUPGOR-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-REMUPGAC-PAG998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-IOFPGORI-CAP998     PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD2-C-IOFPGAC-PAG998      PIC S9(13)V9(02) COMP-3  .
NEW--> 01      HR03CAD3                                                .
NEW-->      02 HR03CAD3-D-CH-TIPO             PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD3-D-CH-NUMERO           PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD3-D-ESPECIE             PIC X(001)               .
NEW-->      02 HR03CAD3-D-CONTROLE            PIC X(008)               .
NEW-->      02 HR03CAD3-D-DTEVENTO            PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD3-D-CODLANC             PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD3-D-DTCONTAB            PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD3-D-DTACERT             PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD3-D-IDENTDC             PIC X(001)               .
NEW-->      02 HR03CAD3-D-DEBCRED             PIC X(001)               .
NEW-->      02 HR03CAD3-D-VLPRIORI            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLPRICOR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLMUTORI            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLMUTCOR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLPROORI            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLPROCOR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLPRMUORI           PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLPRMUCOR           PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-VLAUXIL             PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-JURORIG             PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-CORRORIG            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-CORRCORRI           PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-PROAORIG            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-PROACORR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-ASSIORIG            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-ASSICORR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-LAUDORIG            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-LAUDCORR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-MEDIORIG            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-MEDICORR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-REMUORIG            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-REMUCORR            PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-IOFORIG             PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-IOFCORR             PIC S9(13)V9(02) COMP-3  .
NEW-->      02 HR03CAD3-D-AGDEBCRED           PIC S9(05)V      COMP-3  .
NEW-->      02 HR03CAD3-D-RZDEBCRED           PIC S9(05)V      COMP-3  .
NEW-->      02 HR03CAD3-D-CTDEBCRED           PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD3-D-DTMORA              PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD3-D-LANCMORA            PIC S9(03)V      COMP-3  .
NEW-->      02 HR03CAD3-D-DBCRMORA            PIC X(001)               .
NEW-->      02 HR03CAD3-D-NUMMORA             PIC S9(07)V      COMP-3  .
NEW-->      02 HR03CAD3-D-DTACERTO            PIC S9(09)V      COMP-3  .
NEW-->      02 HR03CAD3-D-CODACERT            PIC S9(03)V      COMP-3  .
NEW-->*
NEW--> 01  HR-SQL-DATC                 PIC X(010).
NEW--> 01  FILLER                        REDEFINES HR-SQL-DATC.
NEW-->     03  HR-SQL-DATC-EUR-DD      PIC X(002).
NEW-->     03  HR-SQL-DATC-EUR-P1      PIC X(001).
NEW-->     03  HR-SQL-DATC-EUR-MM      PIC X(002).
NEW-->     03  FILLER                  PIC X(001).
NEW-->     03  HR-SQL-DATC-EUR-YY      PIC X(004).
NEW--> 01  FILLER                        REDEFINES HR-SQL-DATC.
NEW-->     03  HR-SQL-DATC-USA-MM      PIC X(002).
NEW-->     03  FILLER                  PIC X(001).
NEW-->     03  HR-SQL-DATC-USA-DD      PIC X(002).
NEW-->     03  FILLER                  PIC X(001).
NEW-->     03  HR-SQL-DATC-USA-YY      PIC X(004).
NEW--> 01  FILLER                        REDEFINES HR-SQL-DATC.
NEW-->     03  HR-SQL-DATC-ISO-YY      PIC X(004).
NEW-->     03  FILLER                  PIC X(001).
NEW-->     03  HR-SQL-DATC-ISO-MM      PIC X(002).
NEW-->     03  FILLER                  PIC X(001).
NEW-->*
NEW--> 01  HR-DATC-YMD                 PIC  9(08).
NEW--> 01  FILLER                       REDEFINES HR-DATC-YMD.
NEW-->     03  HR-DATC-YMD-YY          PIC X(004).
NEW-->     03  HR-DATC-YMD-MM          PIC X(002).
NEW-->     03  HR-DATC-YMD-DD          PIC X(002).
      *---------------------------------------------------------------*
      *  AREA RESERVADA PARA STARTAR O  PROGRAMA FNEB3BOO             *
      *---------------------------------------------------------------*
       01  WRK-FNEBWBOO.

       COPY 'FNEBWBOO'.


       LINKAGE SECTION.
      *****************

      ********   IO-PCB   ********
       01  IO-PCB.
           03  IO-TERNAME.
               05  IO-TERNAME1         PIC X.
           03  FILLER                  PIC X(2).
           03  IO-STATUS               PIC X(2).
           03  FILLER                  PIC X(12).
           03  IO-MODNAME              PIC X(8).

      ********   ALT-PCB   ********
       01  ALT-PCB.
           03  ALT-TERNAME             PIC X(8).
           03  FILLER                  PIC X(2).
           03  ALT-STATUS              PIC X(2).


       PROCEDURE DIVISION.
      ********************

       0010-INICIO.
      *************

NEW-->     ENTRY 'DLITCBL' USING
NEW-->                     IO-PCB
NEW-->                     ALT-PCB.
           MOVE  GU              TO         WRK-FUNCAO.

       0020-LER-MSG-INPUT.
      ********************

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

           IF RETURN-CODE EQUAL 04 OR 12
              GOBACK
           END-IF.

           MOVE 'RUCAT630'             TO  WRK-TELA.
           MOVE SPACES                 TO  OUT-DADOS630.

JAPI=E*    IF  IN-PF630   EQUAL  'B'
JAPI=E*        MOVE  IN-SENHAS630     TO  OUT-SENHAS630
JAPI=E*        MOVE 'DIGITE OS CAMPOS E TECLE ENTER' TO OUT-MENSAGEM630
JAPI=E*        PERFORM  0150-INSERE-TELA-RUCAT630
JAPI=E*        GO TO 0020-LER-MSG-INPUT.
JAPI=E*
           IF  WRK-TRANSACAO  NOT EQUAL  'RUCA6630'
               MOVE  IN-SENHAS630     TO  OUT-SENHAS630
               MOVE 'DIGITE OS CAMPOS E TECLE ENTER' TO OUT-MENSAGEM630
               PERFORM  0150-INSERE-TELA-RUCAT630
               GO TO 0020-LER-MSG-INPUT.

           MOVE  ATR-MOD       TO  OUT-ATR-CHA630
      *****                        OUT-ATR-DIG630
                                   OUT-ATR-ORD630
                                   OUT-ATR-S-N630.

           MOVE  SPACES        TO  WRK-INCONS.
           MOVE  ZEROS         TO  WRK-SOMATORIA.
           MOVE  ZEROS         TO  WRK-ORIGEM-AUX.
       0030-CONSISTE-DADOS.
      *********************


JAPI=A     IF  IO-MODNAME  EQUAL 'RUCAT631'
               IF  IN-PF630 EQUAL '9'
                   MOVE  IN-SENHAS631   TO  OUT-SENHAS630
JAPI=A             MOVE 'RUCAT630'      TO  WRK-TELA
JAPI=A             PERFORM 0150-INSERE-TELA-RUCAT630
                   GO  TO  0020-LER-MSG-INPUT
               ELSE

7C2603         IF  IN-PF630 EQUAL '4'
7C2603             PERFORM 0315-PASSA-CONTROLE-RUCA7951
7C2603             GO  TO  0020-LER-MSG-INPUT
7C2603         END-IF

STE161         IF  IN-PF630 EQUAL '7'
STE161             PERFORM 0314-PASSA-CONTROLE-RUCA6655
STE161             GO  TO  0020-LER-MSG-INPUT
STE161         END-IF
RUSSO1         IF  IN-PF630 EQUAL '6'
RUSSO1             PERFORM 0313-PASSA-CONTROLE-RUCA6690
RUSSO1             GO  TO  0020-LER-MSG-INPUT
RUSSO1         END-IF
WIP001         IF  IN-PF630 EQUAL '8'
WIP001             PERFORM 0312-PASSA-CONTROLE-RUCA6650
WIP001             GO  TO  0020-LER-MSG-INPUT
WIP001         END-IF
BR1020         IF  IN-PF630 EQUAL 'E'
BR1020             PERFORM 0311-PASSA-CONTROLE-RUCA6682
BR1020             GO  TO  0020-LER-MSG-INPUT
BR1020         END-IF
               IF  IN-PF630 EQUAL 'D'
CET                MOVE  IN-SENHAS631        TO  WRK-CTR-SENHAS-6680
CET                MOVE ZEROS                TO  WRK-CTR-NOPCAO-6680
CET                MOVE WRK-IN-CHAVE631      TO  WRK-CTR-CHAVE-6680
CET                MOVE IN-ORDEM631          TO  WRK-CTR-ORDEM-6680
CET                MOVE IN-IND631            TO  WRK-CTR-INDICA-6680
CET                PERFORM 0309-PASSA-CONTROLE-RUCA6680
CET                GO  TO  0020-LER-MSG-INPUT
               ELSE
                   MOVE  WRK-IN-CHAVE631 TO WRK-START-CHAVE6632
                   MOVE  IN-ORDEM631    TO  START-ORDEM6632
                   MOVE  IN-IND631      TO  START-IND6632
JAPI=A             PERFORM 0160-PASSA-CONTROLE-RUCA6632
                   GO  TO  0020-LER-MSG-INPUT
               END-IF
               END-IF
           END-IF.

       0050-CONSISTE-DADOS-TECLADOS.
      ******************************

           IF  (WRK-IN-CHAVE630 IS NOT NUMERIC) OR
               (WRK-IN-CHAVE630 EQUAL ZEROS)
               MOVE ATR-LUMI TO OUT-ATR-CHA630
               MOVE '*'      TO WRK-INCONS
           END-IF.

           CALL 'POOL0432' USING WRK-IN-CHAVE630 WRK-DIGITO WRK-TAMANHO.
      **** IF  WRK-DIGITO NOT EQUAL IN-DIG630
      ****     MOVE ATR-LUMI TO OUT-ATR-DIG630
           MOVE  WRK-DIGITO  TO IN-DIG630

           IF  IN-ORDEM630 IS NOT NUMERIC
               MOVE ATR-LUMI TO OUT-ATR-ORD630
               MOVE '*'      TO WRK-INCONS.

           IF  IN-S-N630 EQUAL 'S' OR 'N' OR ' '
               NEXT SENTENCE
           ELSE
               MOVE ATR-LUMI TO OUT-ATR-S-N630
               MOVE '*'      TO WRK-INCONS.

           IF  WRK-INCONS EQUAL '*'
               MOVE IN-SENHAS630    TO  OUT-SENHAS630
               MOVE WRK-IN-CHAVE630 TO  WRK-OUT-CHAVE630-N
               MOVE IN-DIG630       TO  OUT-DIG630
               MOVE IN-ORDEM630     TO  OUT-ORDEM630
               MOVE IN-S-N630       TO  OUT-S-N630
               MOVE MENS-INC        TO  OUT-MENSAGEM630
JAPI=A         MOVE 'RUCAT630'      TO  WRK-TELA
JAPI=A         PERFORM 0150-INSERE-TELA-RUCAT630


       0060-PESQUISA-DB-CADASTRO.
      ***************************

           MOVE  WRK-IN-CHAVE630 TO SSA-CHAVE.

OLD-->*    CALL 'CBLTDLI' USING GHU CAD-PCB DB-CAD SSA1-CAD.
NEW-->     MOVE '002' TO HR-CALL-NUM
NEW-->     MOVE 'U' TO HR-CALL-ID
NEW-->     MOVE HR-SSA011-C-CH-CONTR TO CCONTR OF RUFIV002
ACT006     MOVE SPACES               TO CCTRO-CUSTO OF RUFIV002
BS0214     MOVE 'CRUR'               TO WRK-CCTRO-CUSTO-AUX
NEW-->     MOVE HR-SSA011-C-CH-CONTR TO HR-003-C-CH-CONTR
NEW-->     MOVE 'Y' TO HR-PERFORM-STATUS
NEW-->     PERFORM HR-SELECT-RUFIV002
VERI-I
VERI-I     IF      SQLCODE NOT            EQUAL 0
VERI-I       IF    SQLCODE                EQUAL 100
VERI-I             MOVE    ATR-POS        TO    OUT-ATR-CHA630
VERI-I             MOVE    IN-SENHAS630   TO    OUT-SENHAS630
VERI-I             MOVE    IN-DIG630      TO    OUT-DIG630
VERI-I             MOVE    IN-ORDEM630    TO    OUT-ORDEM630
VERI-I             MOVE    IN-S-N630      TO    OUT-S-N630
VERI-I             MOVE    MENS-RUFIV002  TO    OUT-MENSAGEM630
VERI-I             MOVE    'RUCAT630'     TO    WRK-TELA
VERI-I             PERFORM 0150-INSERE-TELA-RUCAT630
VERI-I             GO  TO  0020-LER-MSG-INPUT
VERI-I       ELSE
VERI-I             MOVE    'DB2'               TO    ERR-TIPO-ACESSO
VERI-I             MOVE    'CONTRATO_RURAL'    TO    ERR-DBD-TAB
VERI-I             MOVE    SPACES              TO    ERR-SEGM
VERI-I             MOVE    'SELECT'            TO    ERR-FUN-COMANDO
VERI-I             MOVE    SQLCODE             TO    ERR-SQL-CODE
VERI-I             MOVE    '1001'              TO    ERR-LOCAL
VERI-I             PERFORM 0170-ROTINA-DE-ERRO.
VERI-I
ECR
ECR          PERFORM 0166-ACESSAR-RURCV040
ECR          PERFORM 0167-ACESSAR-RURCV037
ECR          PERFORM 0168-ACESSAR-FNEB3BOO-CONSULTA
ECR
HE0609     IF    CCONTR OF RUFIV002     EQUAL 1 OR 3 OR 4 OR 11 OR 65
HE0609                                  OR 340 OR 577 OR 1155 OR 1200
HE0609           MOVE    ATR-POS        TO    OUT-ATR-CHA630
HE0609           MOVE    IN-SENHAS630   TO    OUT-SENHAS630
HE0609           MOVE    WRK-IN-CHAVE630 TO   WRK-OUT-CHAVE630-N
HE0609           MOVE    IN-DIG630      TO    OUT-DIG630
HE0609           MOVE    IN-ORDEM630    TO    WRK-OUT-ORDEM630-N
HE0609           MOVE    IN-S-N630      TO    OUT-S-N630
HE0609           MOVE    MENS-RUFIV002  TO    OUT-MENSAGEM630
HE0609           MOVE    'RUCAT630'     TO    WRK-TELA
HE0609           PERFORM 0150-INSERE-TELA-RUCAT630
HE0609           GO  TO  0020-LER-MSG-INPUT
HE0609     END-IF.

           IF WRK-COD-USER    EQUAL   7028237
              IF  (CGARTD OF RUFIV002 EQUAL 700)
                   NEXT SENTENCE
              ELSE
                   MOVE    ATR-POS        TO    OUT-ATR-CHA630
                   MOVE    IN-SENHAS630   TO    OUT-SENHAS630
                   MOVE    WRK-IN-CHAVE630 TO   WRK-OUT-CHAVE630-N
                   MOVE    IN-ORDEM630    TO    OUT-ORDEM630
                   MOVE    IN-S-N630      TO    OUT-S-N630
                   MOVE    MENS-BANEB     TO    OUT-MENSAGEM630
                   MOVE    'RUCAT630'     TO    WRK-TELA
                   PERFORM 0150-INSERE-TELA-RUCAT630
                   GO  TO  0020-LER-MSG-INPUT.
VERI-I
NEW-->     MOVE SQLCODE TO HR-PRIMARY-SQLCODE
NEW-->     PERFORM HR-COND-SELECT-RUCAB003
VERI-I
VERI-I     IF      SQLCODE NOT            EQUAL 0
VERI-I       IF    SQLCODE                EQUAL 100
VERI-I             MOVE    ATR-POS        TO    OUT-ATR-CHA630
VERI-I             MOVE    IN-SENHAS630   TO    OUT-SENHAS630
VERI-I             MOVE    WRK-IN-CHAVE630 TO   WRK-OUT-CHAVE630-N
VERI-I             MOVE    IN-DIG630      TO    OUT-DIG630
VERI-I             MOVE    IN-ORDEM630    TO    OUT-ORDEM630
VERI-I             MOVE    IN-S-N630      TO    OUT-S-N630
VERI-I             MOVE    MENS-RUCAB003  TO    OUT-MENSAGEM630
VERI-I             MOVE    'RUCAT630'     TO    WRK-TELA
VERI-I             PERFORM 0150-INSERE-TELA-RUCAT630
VERI-I       ELSE
VERI-I             MOVE    'DB2'               TO    ERR-TIPO-ACESSO
VERI-I             MOVE    'RUCA_CAD_ROOT'     TO    ERR-DBD-TAB
VERI-I             MOVE    SPACES              TO    ERR-SEGM
VERI-I             MOVE    'SELECT'            TO    ERR-FUN-COMANDO
VERI-I             MOVE    SQLCODE             TO    ERR-SQL-CODE
VERI-I             MOVE    '1002'              TO    ERR-LOCAL
VERI-I             PERFORM 0170-ROTINA-DE-ERRO.
VERI-I
NEW-->     MOVE HR03CAD1 TO DB-CAD
NEW-->     PERFORM HR-SET-PCBSTAT
NEW-->     PERFORM HR-CLOSE-RUCAB004-003-04
NEW-->     MOVE HR-PCB-AREA TO CAD-PCB.

           IF  CAD-STATUS NOT EQUAL SPACES
               IF  CAD-STATUS EQUAL 'GE'
                   MOVE ATR-POS        TO  OUT-ATR-CHA630
                   MOVE IN-SENHAS630   TO  OUT-SENHAS630
                   MOVE WRK-IN-CHAVE630 TO WRK-OUT-CHAVE630-N
                   MOVE IN-DIG630      TO  OUT-DIG630
                   MOVE IN-ORDEM630    TO  OUT-ORDEM630
                   MOVE MENS-REGNENC   TO  OUT-MENSAGEM630
JAPI=A             MOVE 'RUCAT630'     TO  WRK-TELA
JAPI=A             PERFORM 0150-INSERE-TELA-RUCAT630
                   GO  TO  0020-LER-MSG-INPUT
               ELSE
JAPI=A             MOVE 'DB2'                    TO    ERR-TIPO-ACESSO
JAPI=A             MOVE 'DB2PRD.RUCA_CAD_ROOT'   TO    ERR-DBD-TAB
JAPI=A             MOVE SPACES                   TO    ERR-SEGM
JAPI=A             MOVE 'SELECT'                 TO    ERR-FUN-COMANDO
JAPI=A             MOVE SQLCODE                  TO    ERR-SQL-CODE
JAPI=A             MOVE '02'                     TO    ERR-LOCAL
                   PERFORM 0170-ROTINA-DE-ERRO.

           IF  IN-S-N630  NOT EQUAL 'S'
               IF  IN-PF630 EQUAL '4'
JAPI=A             PERFORM 0090-MONTA-IMPRESSAO-RUCAP631  THRU
JAPI=A                     0100-INSERE-IMPRESSAO-RUCAP631
                   MOVE  IN-SENHAS630   TO  START-SENHAS6632
                   MOVE  WRK-IN-CHAVE630 TO WRK-START-CHAVE6632
                   MOVE  IN-ORDEM630    TO  START-ORDEM6632
                   MOVE  4              TO  START-IND6632
                   GO  TO  0020-LER-MSG-INPUT
               ELSE
JAPI=A             PERFORM 0070-MONTA-TELA-RUCAT631 THRU
JAPI=A                     0080-INSERE-TELA-RUCAT631
                   GO  TO  0020-LER-MSG-INPUT.

      **** PROCEDIMENTOS P/ LIGAR O BIT 22 DO CONTROLE ****

           MOVE  CAD-BYTE  TO  WRK-BYTE.
           MOVE 1 TO WRK-CONV.
           MOVE 1 TO WRK-BIT-BYTE.
           CALL 'POOL0004' USING WRK-CONV       WRK-BYTE
                                 WRK-CONVERTIDO WRK-BIT-BYTE.
           MOVE WRK-CONVERTIDO TO OITOBITS.
           MOVE 1 TO BIT(7).
           MOVE 2 TO WRK-CONV.
           MOVE 8 TO WRK-BIT-BYTE.
           CALL 'POOL0004' USING WRK-CONV OITOBITS
                                 WRK-BYTE WRK-BIT-BYTE.
           MOVE  WRK-BYTE  TO  CAD-BYTE.

NEW-->     MOVE '003' TO HR-CALL-NUM
NEW-->     MOVE 'R' TO HR-CALL-ID
NEW-->     MOVE DB-CAD TO HR03CAD1
NEW-->     MOVE HR-003-C-CH-CONTR TO CCONTR OF RUFIV002
NEW-->     PERFORM HR-UPDATE-RUFIV002
NEW-->     MOVE SQLCODE TO HR-PRIMARY-SQLCODE
NEW-->     PERFORM HR-COND-UPDATE-RUCAB003
NEW-->     PERFORM HR-SET-PCBSTAT
NEW-->     MOVE HR-PCB-AREA TO CAD-PCB.

           IF  CAD-STATUS   NOT EQUAL        SPACES
JAPI=A         MOVE 'DB2'                    TO    ERR-TIPO-ACESSO
JAPI=A         MOVE 'DB2PRD.RUCA_CAD_ROOT'   TO    ERR-DBD-TAB
JAPI=A         MOVE SPACES                   TO    ERR-SEGM
JAPI=A         MOVE 'UPDATE'                 TO    ERR-FUN-COMANDO
JAPI=A         MOVE SQLCODE                  TO    ERR-SQL-CODE
JAPI=A         MOVE '03'                     TO    ERR-LOCAL
               PERFORM 0170-ROTINA-DE-ERRO.

           IF  IN-PF630 EQUAL '4'
JAPI=A         PERFORM  0110-MONTA-IMPRESSAO-RUCAP630 THRU

           MOVE SPACES         TO  OUT-DADOS630.
           MOVE IN-SENHAS630   TO  OUT-SENHAS630.
           MOVE MENS-SITINC    TO  OUT-MENSAGEM630.
JAPI=A     MOVE  'RUCAT630'     TO  WRK-TELA.
JAPI=A     PERFORM 0150-INSERE-TELA-RUCAT630.
           GO  TO  0020-LER-MSG-INPUT.


JAPI=A 0070-MONTA-TELA-RUCAT631.
      **************************

           MOVE  ZEROS                TO  OUT-VRSECUR.

           MOVE  IN-SENHAS630         TO  OUT-SENHAS631.
           MOVE  WRK-IN-CHAVE630      TO  OUT-CHAVE631.
           MOVE  IN-ORDEM630          TO  OUT-ORDEM631.
           MOVE  0                    TO  OUT-IND631.

           MOVE  CAD-CONTASVINC       TO  OUT-EXTRATO631.
           MOVE  '7'                  TO WRK-DIGITO.
           MOVE  WRK-DIGITO           TO  OUT-DIGEXT631.
           MOVE  CAD-CHAVE            TO  OUT-CHAVEOPER631.
           MOVE  IN-DIG630            TO  OUT-DIGCHAVE631.

           MOVE  CAD-AG               TO  OUT-AG631
                                          SSA-AG.
OLD-->*    CALL  'CBLTDLI' USING GU MEAG-PCB DB-MEAG SSA1-AG.
NEW-->     MOVE '004' TO HR-CALL-NUM
NEW-->     MOVE 'U' TO HR-CALL-ID
NEW-->     MOVE HR-SSA031-CDAGENC TO CJUNC-DEPDC OF MESUV001
NEW-->     MOVE HR-SSA031-CDAGENC TO HR-004-CDAGENC
NEW-->     MOVE 'Y' TO HR-PERFORM-STATUS

           IF (CGARTD OF RUFIV002 EQUAL 700)
               MOVE ' CREDITO RURAL  -  BANEB'     TO OUT-CABEC-T631
           ELSE
               MOVE 'C R E D I T O    R U R A L'   TO OUT-CABEC-T631.

NEW-->     PERFORM HR-SELECIONA-AGEN

NEW-->     MOVE SQLCODE TO HR-PRIMARY-SQLCODE
NEW-->     PERFORM HR-SET-PCBSTAT
NEW-->     MOVE HR-PCB-AREA TO MEAG-PCB.
           IF  MEAG-STATUS NOT EQUAL SPACES
               IF  MEAG-STATUS EQUAL 'GE'
                   MOVE  SPACES       TO  OUT-NOMEAG631
                   MOVE  ZEROS        TO  OUT-FILINST631
               ELSE
JAPI=A             MOVE 'DB2'                    TO    ERR-TIPO-ACESSO
JAPI=A             MOVE 'DB2PRD.V02DEPDC_BDSCO'  TO    ERR-DBD-TAB
JAPI=A             MOVE SPACES                   TO    ERR-SEGM
JAPI=A             MOVE 'SELECT'                 TO    ERR-FUN-COMANDO
JAPI=A             MOVE SQLCODE                  TO    ERR-SQL-CODE
JAPI=A             MOVE '04'                     TO    ERR-LOCAL
                   PERFORM 0170-ROTINA-DE-ERRO
           ELSE
               MOVE MEAG-NOMEAG       TO OUT-NOMEAG631
               MOVE MEAG-CODIGO       TO  OUT-FILINST631
                                          WRK-FILIALCGC.

BS0915     MOVE  CAD-NUMEMP           TO  WRK-CAMPO-9.
BS0915     MOVE  WRK-CAMPO-7          TO  OUT-FINAN631.
           MOVE  CAD-CONTASCC         TO  OUT-CONTACOR631.
           MOVE  '7'                  TO WRK-DIGITO.
           CALL  'DIGITO' USING CAD-CONTASCC WRK-DIGITO WRK-TAM.
           MOVE  WRK-DIGITO           TO  OUT-DIGCONTA631.

      *  ALTERA CALCULO DO DIGITO DA CONTA CORRENTE PARA OPERACOES BANEB

           IF (CGARTD OF RUFIV002 EQUAL 700)
              MOVE CAD-AG             TO  WRK-AGEN-BANEB
              MOVE CAD-CONTASCC       TO  WRK-CONT-BANEB
              MOVE 'C'                TO  WRK-DIGI-BANEB
              CALL 'POOL0430' USING WRK-CTABANEB WRK-DIGBANEB
              IF   WRK-DIGBANEB        EQUAL  'P'
                   MOVE '0'            TO  OUT-DIGCONTA631
              ELSE
                   MOVE WRK-DIGBANEB   TO  OUT-DIGCONTA631.

           MOVE  CAD-CGCCPF1          TO  WRK-DESC-CGC-R.
           MOVE  WRK-NUMFILI          TO  OUT-CGC-CPF631.
           MOVE  WRK-CONT             TO  OUT-CONTROLE631.
           MOVE  CAD-PROD             TO  OUT-TIPROD631.
           MOVE  CAD-CAPPRO           TO  OUT-CAPACID631.

RO0419****** ACESSAR BASE DE DADOS CMRU PARA OBTER COD BARRAS CONTRATOS
RO0419     PERFORM 0165-OBTER-CDBARRA

           MOVE  CAD-ATIVIDADE        TO  OUT-ATIV631.
           MOVE  CAD-CODEMB           TO  OUT-EMBAL631.
           MOVE  CAD-FISCALIZACAO     TO  OUT-FISCAL631.
           MOVE  CAD-FINALIDADE       TO  OUT-FINALID631.

           IF  CAD-ORIREC3COD EQUAL ZEROS
               IF  CAD-ORIREC2COD EQUAL ZEROS
                   IF  CAD-ORIREC1COD EQUAL ZEROS
                       IF  CAD-ORIRECCOD EQUAL ZEROS
                           MOVE ZEROS TO OUT-ORIGREC631
                           MOVE ZEROS TO WRK-ORIGEM-AUX
                       ELSE
                           MOVE CAD-ORIRECCOD TO OUT-ORIGREC631
                           MOVE CAD-ORIRECCOD TO WRK-ORIGEM-AUX
                   ELSE
                       MOVE CAD-ORIREC1COD TO OUT-ORIGREC631
               ELSE
                   MOVE CAD-ORIREC2COD TO OUT-ORIGREC631
                   MOVE CAD-ORIREC2COD TO WRK-ORIGEM-AUX
           ELSE
               MOVE CAD-ORIREC3COD TO OUT-ORIGREC631
               MOVE CAD-ORIREC3COD TO WRK-ORIGEM-AUX.

           MOVE  CAD-CLASSPROD        TO  OUT-CLAS631.
           MOVE  CAD-EMPREEND         TO  OUT-EMPREE631.
           MOVE  CAD-FIANCA           TO  OUT-CONV631.
POL128     IF    CAD-SEGURADORA NOT EQUAL ZEROS
POL128           MOVE  CAD-SEGURADORA TO  OUT-SEGR631
POL128     END-IF

           IF  CAD-ORIREC3RPL EQUAL ZEROS
               IF  CAD-ORIREC2RPL EQUAL ZEROS
                   IF  CAD-ORIREC1RPL EQUAL ZEROS
                       IF  CAD-ORIRECRPL EQUAL ZEROS
                           MOVE ZEROS TO OUT-RPL631
                       ELSE
                           MOVE CAD-ORIRECRPL TO OUT-RPL631
                       MOVE CAD-ORIREC1RPL TO OUT-RPL631
               ELSE
                   MOVE CAD-ORIREC2RPL TO OUT-RPL631
           ELSE
               MOVE CAD-ORIREC3RPL TO OUT-RPL631.

           MOVE  CAD-AREATOT          TO  OUT-AREATOT631.
           MOVE  CAD-QTDFIN           TO  OUT-QTDFIN631.
           MOVE  CAD-UNIDADE          TO  OUT-UNID631.

RAQ        IF (CGARTD OF RUFIV002 EQUAL 700)
RAQ           MOVE ' 15.142.490'         TO  OUT-CGCINST631
RAQ           MOVE 15142490              TO  WRK-NUMCGC
RAQ        ELSE
RAQ           MOVE ' 60.746.948'         TO  OUT-CGCINST631
RAQ           MOVE 60746948              TO  WRK-NUMCGC.

           CALL  'POOL0110'  USING  WRK-CGC WRK-CONTROLE.
           MOVE WRK-CONTROLE      TO  OUT-CONTRINST631.
           MOVE  CAD-NUMRECOR         TO  OUT-NUMBCO631.
PW9931     IF  CREFT-RECOR  NOT EQUAL SPACES AND ZEROS
PW9931     END-IF.

BR0118     PERFORM 0301-SELECT-GRUPO
BR0118     MOVE CDECLR-AGCLT-FAMLR    TO  WRK-OUT-NDAP631.
BR0118     MOVE WRK-GP                TO  WRK-OUT-GRUPO631.
BR0118     MOVE  DVALDD-DECLR-FAMLR   TO  WRK-DATAS-C.
BR0118     MOVE  WRK-DD-C             TO  WRK-DD-D.
BR0118     MOVE  WRK-MM-C             TO  WRK-MM-D.
BR0118     MOVE  WRK-AA-C             TO  WRK-AA-D.
BR0118     MOVE  WRK-DATAS-D-R        TO  WRK-OUT-DTVALID631.

           MOVE  CAD-LOCMUN           TO  OUT-CODMUNI631.

7C0219     PERFORM 0310-VERIFICA-RUCAB011
.
.          IF WRK-COUNT-RUCAB011 EQUAL ZEROS
.             MOVE 'N'       TO WRK-OUT-LASTRO631
.          ELSE
              MOVE 'S'       TO WRK-OUT-LASTRO631
7C0219     END-IF

           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  OUT-DTREG631.
           MOVE  CAD-DTCEDULA         TO  WRK-DATAS-R.
           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  OUT-DTCED631.
           MOVE  CAD-DTVCTOFIN        TO  WRK-DATAS-R.
           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  OUT-DTVCTOPRO631.
           MOVE  CAD-DTOPERACAO       TO  WRK-DATAS-R.
           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  OUT-DTVCTO631.

           MOVE  CAD-BYTE-2  TO  WRK-BYTE.
           MOVE 1 TO WRK-CONV.
           MOVE 1 TO WRK-BIT-BYTE.
           CALL 'POOL0004' USING WRK-CONV       WRK-BYTE
                                 WRK-CONVERTIDO WRK-BIT-BYTE.
           MOVE WRK-CONVERTIDO TO OITOBITS.

           IF BIT(6)                  EQUAL '1'
              MOVE  VORCAM            TO OUT-TOTORC631
              MOVE  ZEROS             TO  OUT-TOTORC631
              COMPUTE WRK-SOMATORIA   =   CAD-TOTFIN
                                      +   CAD-RECPRO
              MOVE  WRK-SOMATORIA     TO  OUT-TOTORC631.

      *    BIT  62  LIGADO - REDUCAO 5 PONTOS PERCENTUAIS NA TAXA
      *    DE JUROS E IGPM LIMITADO - SECURITIZACAO II (20 ANOS)

           IF BIT(7)                  EQUAL '1'
              MOVE  'REDUCAO JUROS RES.2963   '   TO
                    OUT-LITSEC20631
           ELSE
              MOVE  SPACES                         TO
                    OUT-LITSEC20631.

           MOVE  CAD-VRCOBPRO         TO  OUT-VRCOB631.
           MOVE  'VR.COBERT.PROAGRO'  TO  OUT-LITERA631.
           MOVE  CAD-TOTFIN           TO  OUT-TOTFIN631.
           MOVE  CAD-RECPRO           TO  OUT-VRREC631.
ECR        MOVE  MENS-ENTER           TO  OUT-MENSAGEM631.
JAPI=A     MOVE  'RUCAT631'           TO  WRK-TELA.
           MOVE  CAD-TITULO           TO  OUT-TPTIT631.
           MOVE  CAD-GARANTIA         TO  OUT-TPGAR631.

ACT115     MOVE  DIMPRE-CONTR-RURAL   TO  WRK-OUT-DIMPRE631.
ACT115     INSPECT WRK-OUT-DIMPRE631 REPLACING ALL '.' BY '/'.

           IF   (WRK-ORIGEM-AUX   GREATER 249 AND LESS 271) OR
                (WRK-ORIGEM-AUX   GREATER 899 AND LESS 951)
                 COMPUTE WRK-SOMATORIA =   (CAD-SDO-JUROS998 -
                                            CAD-SDO-JURPG999)
                                        +  (CAD-SDO-VARIACAO998 -
                                           CAD-SDO-CORREPAG999)
                                        +  (CAD-SDO-PRINCIP999 -
                                            CAD-SDO-PRINCPG999)
           ELSE
                 COMPUTE WRK-SOMATORIA =  (CAD-SDO-JUROS998 -
                                           CAD-SDO-JUROS999)
                                        + (CAD-SDO-CORRECAO998 -
                                           CAD-SDO-CORRECAO999)
                                        +  CAD-SDO-DEVEDOR999.


           MOVE  CAD-SDO-DEVEDOR998   TO  OUT-VRSECUR.

           IF   (WRK-ORIGEM-AUX   GREATER 249  AND LESS 271)  OR
                (WRK-ORIGEM-AUX   GREATER 899  AND LESS 951)
                 MOVE    ZEROS        TO  WRK-VLR-DOLAR
                 MOVE    'VR.FINANC.EM DOLAR'
                                      TO  OUT-LITERA631
                 MOVE    'SLD. DEV. PRINC. EM DOLAR'
                                      TO  OUT-LITER1631
                 PERFORM  200-PESQUISA-CALCULA-DOLAR
           ELSE
                 IF   WRK-IN-CHAVE630   GREATER 9000000
BSI214           AND  WRK-IN-CHAVE630   LESS    9100000
                      MOVE  'DIVIDAS SECURIT.  31/07/99'
                                      TO  OUT-LITER1631
                      PERFORM  250-PESQUISA-DIVIDA-SECU3107
                 ELSE
                      MOVE  'VR. SECURITIZACAO 30/11/95'
                                      TO  OUT-LITER1631
                 END-IF


JAPI=A 0080-INSERE-TELA-RUCAT631.
      ***************************

JAPI=A     CALL  'POOL0660' USING OUT-TELA631 MASC-RUCAT631.
           IF  RETURN-CODE NOT EQUAL ZEROS
               MOVE  RETURN-CODE   TO  ERR-RC
JAPI=A         MOVE  'RUCAT631'    TO  ERR-TELA
               MOVE 05             TO  ERR-LOC
               MOVE  MENS-ERR      TO  ERR-TEXTO
               MOVE  'APL'         TO  ERR-TIPO-ACESSO
JAPI=A         MOVE  'RUCA6630'    TO  ERR-PGM
               PERFORM 0170-ROTINA-DE-ERRO.

           MOVE    ISRT                TO   WRK-FUNCAO.
           MOVE    OUT-TELA631         TO   WRK-MENSAGEM.
           MOVE   'RUCAT631'           TO   WRK-TELA.


JAPI=A 0090-MONTA-IMPRESSAO-RUCAP631.

RE0219     PERFORM 0310-VERIFICA-RUCAB011

RE0219     IF WRK-COUNT-RUCAB011 EQUAL ZEROS
RE0219        MOVE 'N'       TO WRK-IMP-LASTRO631
RE0219     ELSE
RE0219        MOVE 'S'       TO WRK-IMP-LASTRO631
RE0219     END-IF

           MOVE  CAD-LOCMUN           TO  IMP-CODMUNI631.
           MOVE  CAD-CONTASVINC       TO  IMP-EXTRATO631.
           MOVE  '7'                  TO  WRK-DIGITO.
           CALL  'DIGITO' USING CAD-CONTASVINC WRK-DIGITO WRK-TAM.
           MOVE  WRK-DIGITO           TO  IMP-DIGEXT631.
           MOVE  CAD-CHAVE            TO  WRK-IMP-CHAVEOPER631.
           MOVE  IN-DIG630            TO  IMP-DIGCHAVE631.

           MOVE  CAD-AG               TO  IMP-AG631
                                          SSA-AG.
OLD-->*    CALL  'CBLTDLI' USING GU MEAG-PCB DB-MEAG SSA1-AG.
NEW-->     MOVE '006' TO HR-CALL-NUM
NEW-->     MOVE HR-SSA031-CDAGENC TO CJUNC-DEPDC OF MESUV001
NEW-->     MOVE HR-SSA031-CDAGENC TO HR-004-CDAGENC
NEW-->     MOVE 'Y' TO HR-PERFORM-STATUS

           IF (CGARTD OF RUFIV002 EQUAL 700)
               MOVE '  BANCO  BANEB  S/A '     TO IMP-CABEC631
           ELSE
               MOVE ' BANCO BRADESCO  S/A'     TO IMP-CABEC631.

NEW-->     PERFORM HR-SELECIONA-AGEN
NEW-->     MOVE SQLCODE TO HR-PRIMARY-SQLCODE
NEW-->     MOVE HRMEAG05 TO DB-MEAG
NEW-->     PERFORM HR-SET-PCBSTAT
NEW-->     MOVE HR-PCB-AREA TO MEAG-PCB.
           IF  MEAG-STATUS NOT EQUAL SPACES
               IF  MEAG-STATUS EQUAL 'GE'
                   MOVE  SPACES       TO  IMP-NOMEAG631
                   MOVE  ZEROS        TO  IMP-FILINST631
               ELSE
JAPI=A             MOVE 'DB2'                    TO    ERR-TIPO-ACESSO
JAPI=A             MOVE 'DB2PRD.V02DEPDC_BDSCO'  TO    ERR-DBD-TAB
JAPI=A             MOVE 'SELECT'                 TO    ERR-FUN-COMANDO
JAPI=A             MOVE SQLCODE                  TO    ERR-SQL-CODE
JAPI=A             MOVE '07'                     TO    ERR-LOCAL
                   PERFORM 0170-ROTINA-DE-ERRO
           ELSE
               MOVE MEAG-NOMEAG       TO IMP-NOMEAG631
               MOVE MEAG-CODIGO       TO  IMP-FILINST631
                                          WRK-FILIALCGC.

BS0915     MOVE  CAD-NUMEMP           TO  WRK-CAMPO-9.
BS0915     MOVE  WRK-CAMPO-7          TO  IMP-FINAN631.
           MOVE  CAD-NOMEMUT          TO  IMP-NOME631.
           MOVE  CAD-CONTASCC         TO  IMP-CONTACOR631.
           MOVE  '7'                  TO WRK-DIGITO.
           CALL  'DIGITO' USING CAD-CONTASCC WRK-DIGITO WRK-TAM.
           MOVE  WRK-DIGITO           TO  IMP-DIGCONTA631.

      *  ALTERA CALCULO DO DIGITO DA CONTA CORRENTE PARA OPERACOES BANEB

           IF (CGARTD OF RUFIV002 EQUAL 700)
              MOVE CAD-AG             TO  WRK-AGEN-BANEB
              MOVE 'C'                TO  WRK-DIGI-BANEB
              CALL 'POOL0430' USING WRK-CTABANEB WRK-DIGBANEB
              IF   WRK-DIGBANEB        EQUAL  'P'
                   MOVE '0'            TO  IMP-DIGCONTA631
              ELSE
                   MOVE WRK-DIGBANEB   TO  IMP-DIGCONTA631.

           MOVE  CAD-CGCCPF1          TO  WRK-DESC-CGC-R.
           MOVE  WRK-NUMFILI          TO  IMP-CGC-CPF631.
           MOVE  WRK-CONT             TO  IMP-CONTROLE631.
           MOVE  CAD-PROD             TO  IMP-TIPROD631.
           MOVE  CAD-CATMUT           TO  IMP-CATEGORIA631.
           MOVE  CAD-CAPPRO           TO  IMP-CAPACID631.
           MOVE  CAD-ATIVIDADE        TO  IMP-ATIV631.
           MOVE  CAD-CODEMB           TO  IMP-EMBAL631.
           MOVE  CAD-FISCALIZACAO     TO  IMP-FISCAL631.
           MOVE  CAD-FINALIDADE       TO  IMP-FINALID631.

           IF  CAD-ORIREC3COD EQUAL ZEROS
               IF  CAD-ORIREC2COD EQUAL ZEROS
                   IF  CAD-ORIREC1COD EQUAL ZEROS
                           MOVE ZEROS TO IMP-ORIGREC631
                           MOVE ZEROS TO WRK-ORIGEM-AUX
                       ELSE
                           MOVE CAD-ORIRECCOD TO IMP-ORIGREC631
                           MOVE CAD-ORIRECCOD TO WRK-ORIGEM-AUX
                   ELSE
                       MOVE CAD-ORIREC1COD TO IMP-ORIGREC631
                       MOVE CAD-ORIREC1COD TO WRK-ORIGEM-AUX
               ELSE
                   MOVE CAD-ORIREC2COD TO IMP-ORIGREC631
                   MOVE CAD-ORIREC2COD TO WRK-ORIGEM-AUX
           ELSE
               MOVE CAD-ORIREC3COD TO IMP-ORIGREC631
               MOVE CAD-ORIREC3COD TO WRK-ORIGEM-AUX.

           MOVE  CAD-CLASSPROD        TO  IMP-CLAS631.
           MOVE  CAD-EMPREEND         TO  IMP-EMPREE631.
           MOVE  CAD-FIANCA           TO  IMP-CONV631.
POL128     IF    CAD-SEGURADORA NOT EQUAL ZEROS
POL128           MOVE  CAD-SEGURADORA TO  IMP-SEGR631
POL128     END-IF
           IF   (WRK-ORIGEM-AUX   GREATER 249  AND LESS  271)  OR
                (WRK-ORIGEM-AUX   GREATER 899  AND LESS  951)
                 MOVE    ZEROS        TO  WRK-VLR-DOLAR
                 MOVE    'VR.FINANC.EM DOLAR'
                                      TO  IMP-LITERA631
                 MOVE    'SLD. DEV. PRINC. EM DOLAR'
                                      TO  IMP-LITER1631
                 PERFORM  200-PESQUISA-CALCULA-DOLAR
           ELSE
                 MOVE  'VR.COBERT.PROAGRO'
                                      TO  IMP-LITERA631
                 MOVE  'VR. SECURITIZACAO 30/11/95'
                                      TO  IMP-LITER1631
                 MOVE  CAD-VRCOBPRO
                                      TO  IMP-VRCOB631
                 MOVE  CAD-SDO-DEVEDOR998   TO  IMP-VRSECUR.

           IF  WRK-IN-CHAVE630   GREATER 9000000
BSI214     AND WRK-IN-CHAVE630   LESS    9100000
               MOVE  'DIVIDAS SECURIT.  31/07/99'
                                TO  IMP-LITER1631
           END-IF.

           IF  CAD-ORIREC3RPL EQUAL ZEROS
               IF  CAD-ORIREC2RPL EQUAL ZEROS
                   IF  CAD-ORIREC1RPL EQUAL ZEROS
                       IF  CAD-ORIRECRPL EQUAL ZEROS
                           MOVE ZEROS TO IMP-RPL631
                       ELSE
                           MOVE CAD-ORIRECRPL TO IMP-RPL631
                   ELSE
                       MOVE CAD-ORIREC1RPL TO IMP-RPL631
               ELSE
                   MOVE CAD-ORIREC2RPL TO IMP-RPL631
           ELSE
               MOVE CAD-ORIREC3RPL TO IMP-RPL631.

           MOVE  CAD-AREATOT          TO  IMP-AREATOT631.
           MOVE  CAD-QTDFIN           TO  IMP-QTDFIN631.
           MOVE  CAD-UNIDADE          TO  IMP-UNID631.

RAQ        IF (CGARTD OF RUFIV002 EQUAL 700)
RAQ           MOVE 15142490              TO  WRK-NUMCGC
RAQ        ELSE
RAQ           MOVE ' 60.746.948'         TO  IMP-CGCINST631
RAQ           MOVE 60746948              TO  WRK-NUMCGC.

           CALL  'POOL0110'  USING  WRK-CGC WRK-CONTROLE.
           MOVE WRK-CONTROLE      TO  IMP-CONTRINST631.
           MOVE  CAD-NUMRECOR         TO  IMP-NUMBCO631.
PW9931     IF  CREFT-RECOR  NOT EQUAL SPACES AND ZEROS
PW9931         MOVE CREFT-RECOR       TO  WRK-IMP-NUMBCO631
PW9931     END-IF.

BR0118     PERFORM 0301-SELECT-GRUPO
BR0118     MOVE CDECLR-AGCLT-FAMLR    TO  WRK-IMP-NDAP631.
BR0118     MOVE WRK-GP                TO  WRK-IMP-GRUPO631.
BR0118     MOVE  DVALDD-DECLR-FAMLR   TO  WRK-DATAS-C.
BR0118     MOVE  WRK-DD-C             TO  WRK-DD-D.
BR0118     MOVE  WRK-MM-C             TO  WRK-MM-D.
BR0118     MOVE  WRK-AA-C             TO  WRK-AA-D.
BR0118     MOVE  WRK-DATAS-D-R        TO  WRK-IMP-DTVALID631.

           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  IMP-DTREG631.
           MOVE  CAD-DTCEDULA         TO  WRK-DATAS-R.
           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  IMP-DTCED631.
           MOVE  CAD-DTVCTOFIN        TO  WRK-DATAS-R.
           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  IMP-DTVCTOPRO631.
           MOVE  CAD-DTOPERACAO       TO  WRK-DATAS-R.
           MOVE  CORR  WRK-DATAS      TO  WRK-INV-DATAS.
           MOVE  WRK-INV-DATAS-R      TO  IMP-DTVCTO631.
           MOVE  CAD-BYTE-2           TO  WRK-BYTE.
           MOVE  1                    TO  WRK-CONV.
           MOVE  1                    TO  WRK-BIT-BYTE.
           CALL 'POOL0004'            USING WRK-CONV       WRK-BYTE
                                            WRK-CONVERTIDO WRK-BIT-BYTE.
           MOVE WRK-CONVERTIDO        TO OITOBITS.

           IF BIT(6)                  EQUAL '1'
              MOVE  VORCAM            TO  IMP-TOTORC631
           ELSE
                                      +   CAD-RECPRO
              MOVE  WRK-SOMATORIA     TO  IMP-TOTORC631.

      *    BIT  62  LIGADO - REDUCAO 5 PONTOS PERCENTUAIS NA TAXA
      *    DE JUROS E IGPM LIMITADO - SECURITIZACAO II (20 ANOS)

           IF BIT(7)                  EQUAL '1'
              MOVE  'REDUCAO JUROS RES.2963   '   TO
                    IMP-LITSEC20631
           ELSE
              MOVE  SPACES                        TO
                    IMP-LITSEC20631.

           MOVE  CAD-TOTFIN           TO  IMP-TOTFIN631.
           MOVE  CAD-RECPRO           TO  IMP-VRREC631.
JAPI=A     MOVE 'RUCAP631'            TO  WRK-MODNAME.

           MOVE  CAD-TITULO           TO  IMP-TPTIT631.
           MOVE  CAD-GARANTIA         TO  IMP-TPGAR631.

ACT115     MOVE  DIMPRE-CONTR-RURAL   TO  WRK-IMP-DIMPRE631.

           IF   (WRK-ORIGEM-AUX   GREATER 249 AND LESS 271) OR
                (WRK-ORIGEM-AUX   GREATER 899 AND LESS 951)
                 COMPUTE WRK-SOMATORIA =   (CAD-SDO-JUROS998 -
                                            CAD-SDO-JURPG999)
                                        +  (CAD-SDO-VARIACAO998 -
                                            CAD-SDO-CORREPAG999)
                                        +  (CAD-SDO-PRINCIP999 -
                                            CAD-SDO-PRINCPG999)
           ELSE
                 COMPUTE WRK-SOMATORIA =  (CAD-SDO-JUROS998 -
                                           CAD-SDO-JUROS999)
                                        + (CAD-SDO-CORRECAO998 -
                                           CAD-SDO-CORRECAO999)
                                        +  CAD-SDO-DEVEDOR999.

           MOVE  WRK-SOMATORIA        TO  IMP-SLDDEV631.


      ********************************
JAPI=A 0100-INSERE-IMPRESSAO-RUCAP631.

JAPI=I     CALL 'POOL1080' USING IO-PCB
JAPI=I                           IN-SENHAS630
JAPI=I                           WRK-IMPRESSORA.
JAPI=I
JAPI=I     IF  RETURN-CODE NOT EQUAL ZEROS
JAPI=I         MOVE  IN-SENHAS630    TO  OUT-SENHAS630
JAPI=I         MOVE  WRK-IN-CHAVE630 TO  WRK-OUT-CHAVE630-N
JAPI=I         MOVE  IN-DIG630       TO  OUT-DIG630
JAPI=I         MOVE  IN-ORDEM630     TO  OUT-ORDEM630
JAPI=I         MOVE  IN-S-N630       TO  OUT-S-N630
JAPI=I         MOVE  MENS-IMPNDISP   TO  OUT-MENSAGEM630
JAPI=I         MOVE  'RUCAT630'      TO  WRK-TELA
JAPI=I         PERFORM  0150-INSERE-TELA-RUCAT630
JAPI=I         GO  TO  0020-LER-MSG-INPUT
JAPI=I     END-IF.

JAPI=A     IF  ALT-TERNAME  NOT  EQUAL  WRK-IMPRESSORA
JAPI=I         MOVE  WRK-IMPRESSORA  TO  WRK-DESTINO
               CALL 'CBLTDLI' USING CHNG ALT-PCB WRK-DESTINO
               IF  ALT-STATUS  EQUAL 'A1'
                   MOVE  WRK-IN-CHAVE630 TO  WRK-OUT-CHAVE630-N
                   MOVE  IN-DIG630       TO  OUT-DIG630
                   MOVE  IN-ORDEM630     TO  OUT-ORDEM630
                   MOVE  IN-S-N630       TO  OUT-S-N630
                   MOVE  MENS-IMPNDISP   TO  OUT-MENSAGEM630
JAPI=A             MOVE  'RUCAT630'      TO  WRK-TELA
JAPI=A             PERFORM  0150-INSERE-TELA-RUCAT630
                   GO  TO  0020-LER-MSG-INPUT
               ELSE
                   IF  ALT-STATUS  NOT  EQUAL SPACES
JAPI=I                 MOVE  'IMS'           TO  ERR-TIPO-ACESSO
                       MOVE  WRK-DESTINO     TO  ERR-DBD-TAB
                       MOVE  CHNG            TO  ERR-FUN-COMANDO
                       MOVE  ALT-STATUS      TO  ERR-STA-CODE
                       MOVE  '08'            TO  ERR-LOCAL
                       MOVE  SPACES          TO  ERR-SEGM
                       PERFORM 0170-ROTINA-DE-ERRO.

JAPI=A     CALL  'POOL0660' USING IMP-TELA631 MASC-RUCAP631.

           IF  RETURN-CODE NOT EQUAL ZEROS
JAPI=A         MOVE  'RUCAP631'    TO  ERR-TELA
               MOVE  09            TO  ERR-LOC
               MOVE  MENS-ERR      TO  ERR-TEXTO
               MOVE  'APL'         TO  ERR-TIPO-ACESSO
JAPI=A         MOVE  'RUCA6630'    TO  ERR-PGM
               PERFORM 0170-ROTINA-DE-ERRO.

           CALL 'CBLTDLI' USING ISRT ALT-PCB IMP-TELA631 WRK-MODNAME.

           IF  ALT-STATUS NOT EQUAL SPACES
JAPI=I         MOVE  'IMS'          TO  ERR-TIPO-ACESSO
               MOVE  WRK-DESTINO    TO  ERR-DBD-TAB
               MOVE  SPACES         TO  ERR-SEGM
               MOVE  ALT-STATUS     TO  ERR-STA-CODE
               MOVE  ISRT           TO  ERR-FUN-COMANDO
               MOVE  '09'           TO  ERR-LOCAL
               PERFORM 0170-ROTINA-DE-ERRO.

           CALL 'CBLTDLI' USING PURG ALT-PCB

           IF  ALT-STATUS NOT EQUAL SPACES
               MOVE  WRK-DESTINO    TO  ERR-DBD-TAB
               MOVE  SPACES         TO  ERR-SEGM
               MOVE  ALT-STATUS     TO  ERR-STA-CODE
               MOVE  PURG           TO  ERR-FUN-COMANDO
               MOVE  '10'           TO  ERR-LOCAL
               PERFORM 0170-ROTINA-DE-ERRO.

JAPI=A 0110-MONTA-IMPRESSAO-RUCAP630.
      *******************************

           MOVE  WRK-IN-CHAVE630 TO WRK-IMP-CHAVE630.
           MOVE  IN-DIG630      TO  IMP-DIG630.
           MOVE  IN-ORDEM630    TO  IMP-ORDEM630.
           MOVE  IN-S-N630      TO  IMP-S-N630.
           MOVE  MENS-SITINC    TO  IMP-MENSAGEM630.
JAPI=A     MOVE  'RUCAP630'     TO  WRK-MODNAME.

JAPI=A 0120-INSERE-IMPRESSAO-RUCAP630.
      ********************************

JAPI=I     CALL 'POOL1080' USING IO-PCB
JAPI=I                           WRK-IMPRESSORA.
JAPI=I
JAPI=I     IF  RETURN-CODE NOT EQUAL ZEROS
JAPI=I         MOVE  IN-SENHAS630    TO  OUT-SENHAS630
JAPI=I         MOVE  WRK-IN-CHAVE630 TO  WRK-OUT-CHAVE630-N
JAPI=I         MOVE  IN-DIG630       TO  OUT-DIG630
JAPI=I         MOVE  IN-ORDEM630     TO  OUT-ORDEM630
JAPI=I         MOVE  IN-S-N630       TO  OUT-S-N630
JAPI=I         MOVE  MENS-IMPNDISP   TO  OUT-MENSAGEM630
JAPI=I         MOVE  'RUCAT630'      TO  WRK-TELA
JAPI=I         PERFORM  0150-INSERE-TELA-RUCAT630
JAPI=I         GO  TO  0020-LER-MSG-INPUT
JAPI=I     END-IF.

JAPI=A     IF  ALT-TERNAME  NOT  EQUAL  WRK-IMPRESSORA
JAPI=I         MOVE  WRK-IMPRESSORA  TO  WRK-DESTINO
               CALL 'CBLTDLI' USING CHNG ALT-PCB WRK-DESTINO
               IF  ALT-STATUS  EQUAL 'A1'
                   MOVE  IN-SENHAS630    TO  OUT-SENHAS630
                   MOVE  WRK-IN-CHAVE630 TO  WRK-OUT-CHAVE630-N
                   MOVE  IN-DIG630       TO  OUT-DIG630
                   MOVE  IN-S-N630       TO  OUT-S-N630
                   MOVE  MENS-IMPNDISP   TO  OUT-MENSAGEM630
JAPI=A             MOVE  'RUCAT630'      TO  WRK-TELA
JAPI=A             PERFORM  0150-INSERE-TELA-RUCAT630
                   GO  TO  0020-LER-MSG-INPUT
               ELSE
                   IF  ALT-STATUS  NOT  EQUAL SPACES
JAPI=I                 MOVE  'IMS'           TO  ERR-TIPO-ACESSO
                       MOVE  WRK-DESTINO     TO  ERR-DBD-TAB
                       MOVE  CHNG            TO  ERR-FUN-COMANDO
                       MOVE  ALT-STATUS      TO  ERR-STA-CODE
                       MOVE  '11'            TO  ERR-LOCAL
                       MOVE  SPACES          TO  ERR-SEGM
                       PERFORM 0170-ROTINA-DE-ERRO.

JAPI=A     CALL  'POOL0660' USING IMP-TELA630 MASC-RUCAP630.

           IF  RETURN-CODE NOT EQUAL ZEROS
               MOVE  RETURN-CODE   TO  ERR-RC
JAPI=A         MOVE  'RUCAP630'    TO  ERR-TELA
               MOVE  13            TO  ERR-LOC
               MOVE  'APL'         TO  ERR-TIPO-ACESSO
JAPI=A         MOVE  'RUCA6630'    TO  ERR-PGM
               PERFORM 0170-ROTINA-DE-ERRO.

           CALL 'CBLTDLI' USING ISRT ALT-PCB IMP-TELA630 WRK-MODNAME.

           IF  ALT-STATUS NOT EQUAL SPACES
JAPI=I         MOVE  'IMS'          TO  ERR-TIPO-ACESSO
               MOVE  WRK-DESTINO    TO  ERR-DBD-TAB
               MOVE  SPACES         TO  ERR-SEGM
               MOVE  ALT-STATUS     TO  ERR-STA-CODE
               MOVE  ISRT           TO  ERR-FUN-COMANDO
               MOVE  '12'           TO  ERR-LOCAL
               PERFORM 0170-ROTINA-DE-ERRO.


JAPI=A 0150-INSERE-TELA-RUCAT630.
      ***************************

JAPI=A     CALL 'POOL0660' USING OUT-TELA630 MASC-RUCAT630.

               MOVE  RETURN-CODE   TO ERR-RC
JAPI=A         MOVE 'RUCAT630'     TO ERR-TELA
               MOVE 18             TO ERR-LOC
               MOVE MENS-ERR       TO ERR-TEXTO
               MOVE 'APL'          TO ERR-TIPO-ACESSO
JAPI=A         MOVE 'RUCA6630'     TO ERR-PGM
               PERFORM 0170-ROTINA-DE-ERRO.

           MOVE    ISRT                TO   WRK-FUNCAO.
           MOVE    OUT-TELA630         TO   WRK-MENSAGEM.
           MOVE   'RUCAT630'           TO   WRK-TELA.


JAPI=A 0160-PASSA-CONTROLE-RUCA6632.
      ******************************

           MOVE    CHNG                TO  WRK-FUNCAO.
           MOVE    START-AREA-RUCA6632 TO  WRK-MENSAGEM.
           MOVE   'RUCA6632'           TO  WRK-TELA.


RO0419 0165-OBTER-CDBARRA  SECTION.
RO0419*---------------------------------------------------------------*
.
.          MOVE  WRK-IN-CHAVE630
.                                          TO
.                                      CCONTR-RURAL-LGADO OF CMRUB001
.
.          EXEC SQL
.              SELECT
.                     CPSSOA_JURID_CONTR
.                    ,CTPO_CONTR_NEGOC
.                    ,NSEQ_CONTR_NEGOC
.                    ,CBARRA_DNAMC_RURAL
.                INTO
.                    :CMRUB001.CPSSOA-JURID-CONTR
.                   ,:CMRUB001.CTPO-CONTR-NEGOC
.                   ,:CMRUB001.NSEQ-CONTR-NEGOC
.                   ,:CMRUB001.CBARRA-DNAMC-RURAL
.                     :WRK-NULL-C-CDBARRAS
.                FROM
.                     DB2PRD.TCONTR_RURAL_CVIVE
.                     CCONTR_RURAL_LGADO
.                                      = :CMRUB001.CCONTR-RURAL-LGADO
.                 AND CTPO_CONTR_NEGOC = 25
.          END-EXEC
.
.          EVALUATE SQLCODE
.            WHEN  ZEROS
.                  IF WRK-NULL-C-CDBARRAS  LESS ZEROS
.                     MOVE ALL '0'
.                                          TO OUT-CDBARR631
.                                             WRK-IMP-CDBARR
                   ELSE
.                     MOVE  CBARRA-DNAMC-RURAL
.                                          OF CMRUB001
.                                          TO OUT-CDBARR631
.                                             WRK-IMP-CDBARR
                   END-IF

.            WHEN +100
.                  MOVE '00000000000000000000000000000000000000000000'
.                                       TO OUT-CDBARR631
.
.            WHEN  OTHER
.                  MOVE 'DB2'           TO ERR-TIPO-ACESSO
.                  MOVE 'TCONTR_RURAL_CVIVE'
.                                       TO ERR-DBD-TAB
.                  MOVE  SPACES         TO ERR-SEGM
.                  MOVE 'SELECT'        TO ERR-FUN-COMANDO
.                  MOVE  SQLCODE        TO ERR-SQL-CODE
.                  MOVE  '1001'         TO ERR-LOCAL
.                  MOVE SPACES          TO OUT-CDBARR631
.                  PERFORM 0170-ROTINA-DE-ERRO
.          END-EVALUATE
.
.          .
.     *---------------------------------------------------------------*
.      0165-99-FIM. EXIT.
RO0419*---------------------------------------------------------------*

ECR   *---------------------------------------------------------------*
       0166-ACESSAR-RURCV040 SECTION.
      *---------------------------------------------------------------*
           MOVE CORIGE-REC   OF RUFIV002 TO CORIGE-REC   OF RURCV040.
           MOVE CATVDD-RURAL OF RUFIV002 TO CATVDD-RURAL OF RURCV040.
           MOVE CFNALD-RURAL OF RUFIV002 TO CFNALD-RURAL OF RURCV040.

           EXEC SQL
               SELECT
                   CGRP_CTBIL_ATUAL
                 , CSGRP_CTBIL
               INTO
                   :RURCV040.CGRP-CTBIL-ATUAL
                 , :RURCV040.CSGRP-CTBIL
               FROM  DB2PRD.V01RAZMES_ORIGREC
               WHERE CORIGE_REC        = :RURCV040.CORIGE-REC
               AND   CATVDD_RURAL      = :RURCV040.CATVDD-RURAL
               AND   CFNALD_RURAL      = :RURCV040.CFNALD-RURAL
           END-EXEC.

           IF  ( SQLCODE                NOT EQUAL ZEROS   )
           OR  ( SQLCODE                =  +100)
           OR  ( SQLWARN0               EQUAL    'W'      )
               MOVE 'DB2'               TO  ERR-TIPO-ACESSO
               MOVE 'SELECT  '          TO  ERR-FUN-COMANDO
               MOVE SQLCODE             TO  ERR-SQL-CODE
               MOVE '0166'              TO  ERR-LOCAL
               MOVE SPACES              TO  ERR-SEGM
               PERFORM 0170-ROTINA-DE-ERRO
           END-IF.

      *---------------------------------------------------------------*
       0166-99-FIM. EXIT.
      *---------------------------------------------------------------*

ECR   *---------------------------------------------------------------*
       0167-ACESSAR-RURCV037 SECTION.
      *---------------------------------------------------------------*

           MOVE CGRP-CTBIL-ATUAL       OF RURCV040
                                       TO CGRP-CTBIL-ATUAL OF RURCV037.
           MOVE CSGRP-CTBIL            OF RURCV040
                                       TO CSGRP-CTBIL      OF RURCV037.
           EXEC SQL
               SELECT
               INTO
                   :RURCV037.CCART-MORA-RURAL
               FROM  DB2PRD.V01RAZAO_MESTRE
               WHERE CGRP_CTBIL_ATUAL  = :RURCV037.CGRP-CTBIL-ATUAL
               AND   CSGRP_CTBIL       = :RURCV037.CSGRP-CTBIL
           END-EXEC.

           IF  ( SQLCODE                NOT EQUAL ZEROS    )
           OR  ( SQLCODE                =  +100)
           OR  ( SQLWARN0               EQUAL    'W'       )
               MOVE 'DB2'               TO  ERR-TIPO-ACESSO
               MOVE 'V01RAZAO_MESTRE'   TO  ERR-DBD-TAB
               MOVE 'SELECT  '          TO  ERR-FUN-COMANDO
               MOVE SQLCODE             TO  ERR-SQL-CODE
               MOVE '0167'              TO  ERR-LOCAL
               MOVE SPACES              TO  ERR-SEGM
               PERFORM 0170-ROTINA-DE-ERRO
           END-IF.

      *---------------------------------------------------------------*
       0167-99-FIM. EXIT.

ECR   *---------------------------------------------------------------*
       0168-ACESSAR-FNEB3BOO-CONSULTA   SECTION.
      *---------------------------------------------------------------*

           INITIALIZE FNEBWBOO-BLOCO-ERRO
                      FNEBWBOO-ENTRADA
                      FNEBWBOO-SAIDA.
           MOVE '2'                       TO FNEBWBOO-E-ACAO.
           MOVE '1'                       TO FNEBWBOO-E-CHAVE.
           MOVE 'RURA'                    TO FNEBWBOO-E-CSIST.
           MOVE 237                       TO FNEBWBOO-E-CBCO.
           MOVE CJUNC-DEPDC OF MESUV001   TO FNEBWBOO-E-CAG-BCRIA.
           MOVE CCTA-CORR   OF RUFIV002   TO FNEBWBOO-E-CCTA-BCRIA-CLI.

FSY319     MOVE CCART-MORA-RURAL OF RURCV037
FSY319                                    TO WRK-CART-X.
FSY319     MOVE WRK-CART-N                TO WRK-CART-P.
FSY319     MULTIPLY WRK-CART-P   BY 10
FSY319                                GIVING WRK-CART-P.
FSY319     MOVE WRK-CART-P                TO WRK-CART-N.

           MOVE SPACES                    TO FNEBWBOO-E-RCOMPL-PRODT.
           MOVE CCONTR OF RUFIV002     TO FNEBWBOO-E-CIDTFD-CONTR-LGADO.
           MOVE ZEROS                  TO FNEBWBOO-E-CPSSOA-JURID-CONTR.
           MOVE ZEROS                  TO FNEBWBOO-E-CTPO-CONTR-NEGOC.
           MOVE ZEROS                  TO FNEBWBOO-E-NSEQ-CONTR-NEGOC.

           MOVE    'FNEB3BOO'          TO  WRK-MODULO.

           CALL    WRK-MODULO    USING    WRK-FNEBWBOO

           IF  FNEBWBOO-E-COD-RETORNO    NOT EQUAL ZEROS
               IF FNEBWBOO-E-COD-RETORNO     EQUAL 16
                  MOVE 'DB2'                    TO ERR-TIPO-ACESSO
                  MOVE SPACES                   TO ERR-SEGM
                  MOVE 'FNEB3BOO'               TO ERR-MODULO
                  PERFORM 0170-ROTINA-DE-ERRO
               ELSE
                  IF CFORMA-LIQDC OF RUFIV002   EQUAL 3
                     MOVE 'NAO'  TO WRK-OUT-DEBI
                     MOVE 'NAO'  TO WRK-OUT-LIMI
                  ELSE
                     MOVE 'SIM'  TO WRK-OUT-DEBI
                     MOVE 'SIM'  TO WRK-OUT-LIMI
                     GO TO 0168-99-FIM
                  END-IF
               END-IF
           END-IF.

           IF  FNEBWBOO-S-CINDCD-OPTE-REENV EQUAL 2
              MOVE 'NAO'   TO  WRK-OUT-DEBI
           ELSE
              MOVE 'SIM'   TO  WRK-OUT-DEBI
           END-IF.

           IF  FNEBWBOO-S-CINDCD-OPTE-CHEQ EQUAL 2
               MOVE 'NAO'  TO  WRK-OUT-LIMI
           ELSE
               MOVE 'SIM'  TO  WRK-OUT-LIMI
           END-IF.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0170-ROTINA-DE-ERRO SECTION.
      *---------------------------------------------------------------*

JAPI=A     MOVE     'RUCA6630'      TO           ERR-PGM
JAPI=A
JAPI=A     IF  ERR-TIPO-ACESSO      EQUAL        'DB2'
JAPI=A         CALL 'POOL7100' USING IO-PCB ERRO-AREA ALT-PCB SQLCA
JAPI=A     ELSE
JAPI=A         CALL 'POOL7100' USING IO-PCB ERRO-AREA ALT-PCB.

           GOBACK.

      *---------------------------------------------------------------*
       170-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       200-PESQUISA-CALCULA-DOLAR SECTION.

      *------------------------*
       201-PESQUISA-FINANC-DOL.
      *------------------------*

           PERFORM  210-LEITURA-DB.

           IF   CAD-STATUS  EQUAL   'GE'
                MOVE   ZEROS  TO   OUT-VRCOB631
                                   IMP-VRCOB631
                                   OUT-VRSECUR
                                   IMP-VRSECUR
                GO            TO   205-00-DOLAR-ATUAL.

           IF ( S025-DEBCRED       EQUAL  2  )  AND
              ( S025-CODLANC       LESS   56 )  AND
              ( S025-VLPRICOR  NOT EQUAL  ZEROS  )
                MOVE  S025-VLPRICOR     TO    WRK-VLPRICOR
                MOVE  S025-MEDIORIG     TO    WRK-MEDIORIG
                GO                      TO    202-MOVE-VL-DOLAR.

              ( S025-CODLANC           EQUAL 325 )  AND
              ( WRK-VLAUXIL-R          EQUAL ZEROS    )
                MOVE  S025-VLAUXIL-R    TO    WRK-VLAUXIL-R.

           GO                           TO    201-PESQUISA-FINANC-DOL.

      *------------------*
       202-MOVE-VL-DOLAR.
      *------------------*

           IF   WRK-MEDIORIG    NOT EQUAL ZEROS
                MOVE  WRK-MEDIORIG      TO    OUT-VRCOB631
                                              IMP-VRCOB631
                GO                      TO    205-00-DOLAR-ATUAL.

           IF   WRK-VLAUXIL-R   NOT EQUAL ZEROS
                GO                      TO    204-CALCULA-DOLAR.

      *-------------------*
       203-PESQUISA-DOLAR.
      *-------------------*
           PERFORM 210-LEITURA-DB.

           IF   CAD-STATUS   EQUAL  'GE' OR 'GB'
                IF WRK-VLAUXIL-R    EQUAL ZEROS
                   MOVE   ZEROS  TO   OUT-VRCOB631
                                      IMP-VRCOB631
                                      OUT-VRSECUR
                                      IMP-VRSECUR
                   GO            TO   200-99-FIM
                ELSE
                   GO            TO   203-EXIT.

           IF S025-DEBCRED           EQUAL 8    AND
              S025-CODLANC           EQUAL 325
              MOVE  S025-VLAUXIL-R    TO    WRK-VLAUXIL-R.

           GO                      TO    203-PESQUISA-DOLAR.

      *------------------*
       203-EXIT.
      *------------------*
      *------------------*

           COMPUTE  WRK-VLPRICOR = ( WRK-VLPRICOR / WRK-VLAUXIL-R ).

           MOVE     WRK-VLPRICOR   TO  OUT-VRCOB631
                                       IMP-VRCOB631.

      *------------------*
       205-00-DOLAR-ATUAL.
      *------------------*
           PERFORM  203-PESQUISA-DOLAR  THRU  203-EXIT.

           IF       WRK-VLAUXIL-R     NOT EQUAL ZEROS
           COMPUTE WRK-SAL = (CAD-SDO-PRINCIP999 - CAD-SDO-PRINCPG999)
           COMPUTE  WRK-VLPRICOR = (WRK-SAL            / WRK-VLAUXIL-R)
           MOVE     WRK-VLPRICOR      TO  OUT-VRSECUR
                                          IMP-VRSECUR
           ELSE
           MOVE     ZEROS             TO  OUT-VRSECUR
                                          IMP-VRSECUR.

       200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       210-LEITURA-DB SECTION.
      *---------------------------------------------------------------*

OLD-->*    CALL  'CBLTDLI'    USING   GHNP
OLD-->*                               CAD-PCB
OLD-->*                               DB-CAD
OLD-->*                               DB-CAD-R
OLD-->*                               SSA04-CAD-NQ.
NEW-->     MOVE '018' TO HR-CALL-NUM
NEW-->     MOVE 'P' TO HR-CALL-ID
NEW-->     PERFORM HR-FETCH-RUCAB004-003-04
NEW-->     MOVE SQLCODE TO HR-PRIMARY-SQLCODE
NEW-->     PERFORM HR-SELECT-RUCAB004
NEW-->     MOVE HR03CAD3 TO DB-CAD-R
NEW-->     PERFORM HR-SET-PCBSTAT
NEW-->     MOVE HR-PCB-AREA TO CAD-PCB.

JAPI=A           MOVE 'DB2'                    TO    ERR-TIPO-ACESSO
JAPI=A           MOVE 'DB2PRD.RUCA_CAD_DEP'    TO    ERR-DBD-TAB
JAPI=A           MOVE SPACES                   TO    ERR-SEGM
JAPI=A           MOVE 'SELECT'                 TO    ERR-FUN-COMANDO
JAPI=A           MOVE SQLCODE                  TO    ERR-SQL-CODE
JAPI=A           MOVE '19'                     TO    ERR-LOCAL
                 PERFORM  300-DESFAZ-DB
                 PERFORM  0170-ROTINA-DE-ERRO.

      *---------------------------------------------------------------*
       210-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       250-PESQUISA-DIVIDA-SECU3107 SECTION.
      *---------------------------------------------------------------*


           MOVE WRK-IN-CHAVE630       TO    B004-CH-CONTR.

           EXEC SQL
                   B004_VLAUXIL
              INTO
                  :RUCAB004.B004-VLAUXIL
              FROM DB2PRD.RUCA_CAD_DEP
                WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR AND
                       B004_CH_TIPO       = 100                     AND
                       B004_DEBCRED       = '9'                     AND
                       B004_DTEVENTO      = '31.07.1999'            AND
                       B004_CODLANC       = 980
           END-EXEC.

           IF    ( SQLCODE   NOT  =    ZEROS AND +100 )        OR
                 ( SQLWARN0       =    'W'            )
                   MOVE 'DB2'                    TO   ERR-TIPO-ACESSO
                   MOVE 'RUCA_CAD_DEP      '     TO   ERR-DBD-TAB
                   MOVE 'SELECT  '               TO   ERR-FUN-COMANDO
                   MOVE SQLCODE                  TO   ERR-SQL-CODE
                   MOVE 0014                     TO   ERR-LOCAL
                   MOVE SPACES                   TO   ERR-SEGM
                   PERFORM 0170-ROTINA-DE-ERRO.

                    MOVE     ZEROS        TO  OUT-VRSECUR
                                              IMP-VRSECUR
           ELSE
                    MOVE     B004-VLAUXIL TO  OUT-VRSECUR
                                              IMP-VRSECUR.


      *---------------------------------------------------------------*
       250-99-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       300-DESFAZ-DB SECTION.
      *---------------------------------------------------------------*

           CALL  'CBLTDLI'  USING  ROLB
                                   IO-PCB.

      *---------------------------------------------------------------*
       300-99-FIM. EXIT.
      *---------------------------------------------------------------*

BR0118 0301-SELECT-GRUPO   SECTION.
BR0118*---------------------------------------------------------------*

           MOVE CGRP-AGCLT-FAMLR OF RUFIV002
                           TO CGRP-AGCLT-FAMLR OF RURCB00D.

           EXEC SQL
           SELECT ITITLO_AGCLT_FAMLR
           INTO   :RURCB00D.ITITLO-AGCLT-FAMLR
           FROM DB2PRD.TGRP_AGCLT_FAMLR
           WHERE CGRP_AGCLT_FAMLR   = :RURCB00D.CGRP-AGCLT-FAMLR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TGRP_AGCLT_FAMLR  ' TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '0001'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
           END-IF.

           IF SQLCODE    EQUAL  +100
              MOVE SPACES TO ITITLO-AGCLT-FAMLR OF RURCB00D
           END-IF

           MOVE ITITLO-AGCLT-FAMLR OF RURCB00D TO WRK-RESUMO.

           IF WRK-RES1 NOT EQUAL SPACES AND
              WRK-RES2 NOT EQUAL SPACES
              MOVE WRK-RES1 TO WRK-GP1
              MOVE '/'      TO WRK-GP2
              MOVE WRK-RES2 TO WRK-GP3
           ELSE
              MOVE SPACES   TO WRK-GP1
              MOVE WRK-RES1 TO WRK-GP2
              MOVE SPACES   TO WRK-GP3
           END-IF.
BR0118*---------------------------------------------------------------*
BR0118 0301-99-FIM. EXIT.
BR0118*---------------------------------------------------------------*
CET    0309-PASSA-CONTROLE-RUCA6680 SECTION.
CET   *---------------------------------------------------------------*
CET
CER        MOVE   'RUCA6630'           TO  WRK-CTR-TRANSACAO-6680
CET        MOVE    CHNG                TO  WRK-FUNCAO.
CET        MOVE   'RUCA6680'           TO  WRK-TELA.
CET        MOVE    WRK-CTR-MSG-6680    TO  WRK-MENSAGEM.
CET
CET   *---------------------------------------------------------------*
CET    0309-99-FIM. EXIT.
CET   *---------------------------------------------------------------*

7C0219******************************************************************
.      0310-VERIFICA-RUCAB011    SECTION.
.     ******************************************************************
.     *
.          MOVE ZEROS                   TO WRK-COUNT-RUCAB011
.          MOVE WRK-IN-CHAVE630         TO CCONTR-CREDT-RURAL
.                                                           OF RUCAB011
.
.          EXEC SQL
.               INTO  :WRK-COUNT-RUCAB011
.               FROM   DB2PRD.TLASTR_PCELA_AGRON_RISCO
.               WHERE  CCONTR_CREDT_RURAL = :RUCAB011.CCONTR-CREDT-RURAL
.          END-EXEC
.
.          IF  (SQLCODE        NOT EQUAL   ZEROS
.          AND                            +100  )
.          OR  (SQLWARN0            EQUAL 'W'   )
.               MOVE 'DB2'              TO ERR-TIPO-ACESSO
.               MOVE 'TLASTR_PCELA_AGR' TO ERR-DBD-TAB
.               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
.               MOVE  SQLCODE           TO ERR-SQL-CODE
.               MOVE '0310'             TO ERR-LOCAL
.               MOVE  SPACES            TO ERR-SEGM
.               PERFORM 0170-ROTINA-DE-ERRO
.          END-IF.
.
.          IF   SQLCODE EQUAL +100
.               MOVE ZEROS TO WRK-COUNT-RUCAB011
.          END-IF.
.
.      0310-99-FIM. EXIT.
7C0219******************************************************************
BR1020*---------------------------------------------------------------*
BR1020 0311-PASSA-CONTROLE-RUCA6682    SECTION.
BR1020*---------------------------------------------------------------*
BR1020
BR1020     MOVE IN-SENHAS631           TO WRK-CTR-SENHAS-6680
BR1020     MOVE WRK-IN-CHAVE631        TO WRK-CTR-CCONTR-N-6682
BR1020     MOVE WRK-I-AGENCIA          TO WRK-CTR-CDAGEN-6682
BR1020     MOVE WRK-I-NOMAG            TO WRK-CTR-DSAGEN-6682
BR1020     MOVE WRK-I-AAFINAN          TO WRK-CTR-CEDULA-6682
BR1020     MOVE WRK-I-NOME             TO WRK-CTR-NOMECL-6682
BR1020     MOVE WRK-I-CC               TO WRK-CTR-CTACOR-6682
BR1020     MOVE WRK-I-CGCCPF           TO WRK-CTR-CNPJCP-6682
BR1020
BR1020     MOVE LENGTH                 OF WRK-CTR-MSG-6682
BR1020                                 TO WRK-CTR-LL-6682
BR1020     MOVE 'RUCA6630'             TO WRK-CTR-TRANSACAO-6682
BR1020     MOVE  CHNG                  TO WRK-FUNCAO.
BR1020     MOVE 'RUCA6682'             TO WRK-TELA.
BR1020     MOVE  WRK-CTR-MSG-6682      TO WRK-MENSAGEM.
BR1020*---------------------------------------------------------------*
BR1020 0311-99-FIM. EXIT.
BR1020*---------------------------------------------------------------*
BR1020
WIP001*---------------------------------------------------------------*
WIP001 0312-PASSA-CONTROLE-RUCA6650    SECTION.
WIP001*---------------------------------------------------------------*
WIP001
WIP001     MOVE IN-SENHAS631           TO WRK-COMU-SENHA
WIP001     MOVE WRK-IN-CHAVE631        TO WRK-COMU-CHNUM-N
WIP001     MOVE IN-DIG630              TO WRK-COMU-CHDIG
WIP001     MOVE WRK-I-AGENCIA          TO WRK-COMU-AGENCIA
WIP001     MOVE WRK-I-NOMAG            TO WRK-COMU-NOMAG
WIP001     MOVE WRK-I-AAFINAN          TO WRK-COMU-AAFINAN
WIP001     MOVE WRK-I-NOME             TO WRK-COMU-NOME
WIP001     MOVE WRK-I-CC               TO WRK-COMU-CC
WIP001     MOVE WRK-I-CGCCPF           TO WRK-COMU-CNPJCPF
WIP001
WIP001     MOVE 'RUCA6630'             TO WRK-COMU-TRANCODE
WIP001     MOVE  CHNG                  TO WRK-FUNCAO.
WIP001     MOVE 'RUCA6650'             TO WRK-TELA.
WIP001
WIP001*---------------------------------------------------------------*
WIP001 0312-99-FIM. EXIT.
WIP001*---------------------------------------------------------------*
WIP001
RUSSO1*---------------------------------------------------------------*
RUSSO1 0313-PASSA-CONTROLE-RUCA6690    SECTION.
RUSSO1*---------------------------------------------------------------*
RUSSO1
RUSSO1     MOVE IN-SENHAS631           TO WRK-COMU-SENHA-6690
RUSSO1     MOVE WRK-I-NROPER           TO WRK-COMU-NROPER-6690
RUSSO1
RUSSO1     MOVE 'RUCA6630'             TO WRK-COMU-TRANCODE-6690
RUSSO1     MOVE  CHNG                  TO WRK-FUNCAO.
RUSSO1     MOVE 'RUCA6690'             TO WRK-TELA.
RUSSO1     MOVE  WRK-COMU-AREA-6690    TO WRK-MENSAGEM.
RUSSO1
RUSSO1*---------------------------------------------------------------*
RUSSO1 0313-99-FIM. EXIT.
RUSSO1*---------------------------------------------------------------*
RUSSO1
STE161 0314-PASSA-CONTROLE-RUCA6655    SECTION.
STE161*---------------------------------------------------------------*
STE161
STE161     MOVE IN-SENHAS631           TO WRK-COMU-SENHA-6655
STE161     MOVE WRK-I-NROPER           TO WRK-COMU-NROPER-6655
STE161     MOVE 'RUCA6630'             TO WRK-COMU-TRANCODE-6655
STE161     MOVE  CHNG                  TO WRK-FUNCAO.
STE161     MOVE 'RUCA6655'             TO WRK-TELA.
STE161     MOVE  WRK-COMU-AREA-6655    TO WRK-MENSAGEM.
STE161
STE161*---------------------------------------------------------------*
STE161 0314-99-FIM. EXIT.
STE161*---------------------------------------------------------------*
7C2603*---------------------------------------------------------------*
7C2603 0315-PASSA-CONTROLE-RUCA7951    SECTION.
7C2603*---------------------------------------------------------------*
7C2603
7C2603     IF WRK-I-ORIREC             NOT EQUAL 100
              MOVE 'OPERACAO NAO POSSUI REORGANIZACAO'
                                       TO OUT-MENSAGEM631
           END-IF
7C2603     MOVE IN-SENHAS631           TO WRK-COMU-SENHAS-7951
7C2603     MOVE WRK-I-NROPER           TO WRK-COMU-NROPER-7951
7C2603     MOVE WRK-I-AGENCIA          TO WRK-COMU-AG-7951
7C2603     MOVE WRK-I-CC               TO WRK-COMU-CC-7951
7C2603     MOVE WRK-I-CGCCPF           TO WRK-COMU-CGCCPF-7951
7C2603     MOVE 'RUCA6630'             TO WRK-COMU-TRANCODE-7951
7C2603     MOVE  CHNG                  TO WRK-FUNCAO.
7C2603     MOVE 'RUCA7951'             TO WRK-TELA.
7C2603     MOVE  WRK-COMU-AREA-7951    TO WRK-MENSAGEM.
7C2603
7C2603*---------------------------------------------------------------*
7C2603 0315-99-FIM. EXIT.
7C2603*---------------------------------------------------------------*
NEW-->*
NEW--> HR-FETCH-RUCAB004-003-04 SECTION.
NEW-->     EXEC SQL
NEW-->       DECLARE HR_RUCAB004_003_04 CURSOR FOR
NEW-->         SELECT
NEW-->           B004_CH_CONTR,
NEW-->           B004_CH_TIPO,
NEW-->           B004_CH_NUMERO
NEW-->         WHERE
NEW-->           (
NEW-->           B004_CH_CONTR = :B004-CH-CONTR
NEW-->           )
NEW-->         ORDER BY
NEW-->           B004_CH_CONTR ,
NEW-->           B004_CH_TIPO ,
NEW-->           B004_CH_NUMERO
NEW-->     END-EXEC
NEW-->     MOVE 'N' TO HR-PERFORM-STATUS
NEW-->     IF  HR-CURSOR-RUCAB004-003-04 NOT = 'OPENED'
NEW-->         MOVE HR-003-C-CH-CONTR TO B004-CH-CONTR
NEW-->         EXEC SQL OPEN HR_RUCAB004_003_04 END-EXEC
NEW-->         MOVE 'OPEN  ' TO HR-CURRENT-SQLSTMT
NEW-->         PERFORM HR-CHECK-SQLCODE
NEW-->         MOVE 'OPENED' TO HR-CURSOR-RUCAB004-003-04
NEW-->         IF  SQLCODE NOT = ZEROS
NEW-->             GO TO HR-FETCH-RUCAB004-003-04-END.
NEW-->     EXEC SQL
NEW-->       FETCH HR_RUCAB004_003_04 INTO
NEW-->         :B004-CH-CONTR,
NEW-->         :B004-CH-NUMERO
NEW-->     END-EXEC
NEW-->     MOVE 'FETCH ' TO HR-CURRENT-SQLSTMT
NEW-->     PERFORM HR-CHECK-SQLCODE
NEW-->     IF  SQLCODE NOT = ZEROS
NEW-->         GO TO HR-FETCH-RUCAB004-003-04-END.
NEW-->     MOVE B004-CH-CONTR TO HR-003-C-CH-CONTR
NEW-->     MOVE B004-CH-TIPO TO HR-003-D-CH-TIPO
NEW-->     MOVE B004-CH-NUMERO TO HR-003-D-CH-NUMERO
NEW-->     MOVE 'Y' TO HR-PERFORM-STATUS.
NEW--> HR-FETCH-RUCAB004-003-04-END. EXIT.
NEW-->*

ALT--> HR-SELECIONA-AGEN   SECTION.

NEW-->     IF  NOT HR-PREVIOUS-OK
               GO TO HR-SELECIONA-AGEN-END.
           MOVE 'N' TO HR-PERFORM-STATUS.

           IF (CGARTD OF RUFIV002 EQUAL 700)
               PERFORM HR-TAB-INTERNA
               PERFORM HR-SELECT-MESUV001.

ALT--> HR-SELECIONA-AGEN-END. EXIT.

ALT--> HR-TAB-INTERNA      SECTION.

           SET WRK-IND TO 1.
           SEARCH  TAB-INTERNA
           AT END
               MOVE +100                     TO SQLCODE
           WHEN WRK-TB-COD(WRK-IND) EQUAL CJUNC-DEPDC OF MESUV001
                MOVE WRK-TB-NOME(WRK-IND)    TO HRMEAG05-NOMEAGE
                MOVE WRK-TB-FILIAL(WRK-IND)  TO HRMEAG05-MEAGCOD
                MOVE CJUNC-DEPDC OF MESUV001 TO HRMEAG05-CDAGENC
                MOVE 'Y'                     TO HR-PERFORM-STATUS
                MOVE ZEROS                   TO SQLCODE.

ALT--> HR-TAB-INTERNA-END. EXIT.

       HR-SELECT-MESUV001  SECTION.

NEW-->       SELECT
NEW-->         IDEPDC_BDSCO,
NEW-->         CFLIAL_CGC
NEW-->       INTO
NEW-->         :IDEPDC-BDSCO,
NEW-->         :CFLIAL-CGC
NEW-->       FROM DB2PRD.V02DEPDC_BDSCO
NEW-->       WHERE
NEW-->         CJUNC_DEPDC = :MESUV001.CJUNC-DEPDC
NEW-->     END-EXEC
NEW-->     MOVE 'SELECT' TO HR-CURRENT-SQLSTMT
NEW-->     PERFORM HR-CHECK-SQLCODE
NEW-->     IF  SQLCODE NOT = ZEROS
NEW-->         GO TO HR-SELECT-MESUV001-END.
NEW-->     MOVE CJUNC-DEPDC OF MESUV001 TO HRMEAG05-CDAGENC
NEW-->     MOVE IDEPDC-BDSCO TO HRMEAG05-NOMEAGE
NEW-->     MOVE CFLIAL-CGC TO HRMEAG05-MEAGCOD
NEW-->     MOVE 'Y' TO HR-PERFORM-STATUS.
NEW--> HR-SELECT-MESUV001-END. EXIT.

NEW-->*
NEW-->     IF  NOT HR-PREVIOUS-OK
NEW-->         GO TO HR-SELECT-RUCAB003-END.
NEW-->     MOVE 'N' TO HR-PERFORM-STATUS
NEW-->     EXEC SQL
NEW-->       SELECT
NEW-->         B003_CONTROL,
NEW-->         B003_NOMUTUAR1,
NEW-->         B003_PESSOA1,
NEW-->         B003_CGCPF1,
NEW-->         B003_NOMUTUAR2,
NEW-->         B003_PESSOA2,
NEW-->         B003_CGCPF2,
NEW-->         B003_NOMUTUAR3,
NEW-->         B003_PESSOA3,
NEW-->         B003_CGCPF3,
NEW-->         B003_NUMDEPEN,
NEW-->         B003_TPDEPEN,
NEW-->         B003_MICREGIAO,
NEW-->         B003_UF,
NEW-->         B003_REGIAO,
NEW-->         B003_EMERG,
NEW-->         B003_VLPROD,
NEW-->         B003_QTDMVR,
NEW-->         B003_INDENIZ,
NEW-->         B003_AGENAVAL,
NEW-->         B003_CTAAVAL,
NEW-->         B003_NOMAVAL,
NEW-->         B003_PESAVAL,
NEW-->         B003_CGCPFAVAL,
NEW-->         B003_ESPBENS,
VERI-I         B003_ORIGREC1,
VERI-I         B003_IDENT1,
NEW-->         B003_DATA1,
NEW-->         B003_ORIGREC2,
NEW-->         B003_IDENT2,
NEW-->         B003_DATA2,
NEW-->         B003_ORIGREC3,
NEW-->         B003_IDENT3,
NEW-->         B003_DATA3,
NEW-->         B003_ORIGREC4,
NEW-->         B003_IDENT4,
NEW-->         B003_DATA4,
NEW-->         B003_PLREFINAM,
NEW-->         B003_SPRGREFIN,
NEW-->         B003_DTREFINAM,
NEW-->         B003_TXREFIN,
NEW-->         B003_NCARTREF,
NEW-->         B003_POSTO,
NEW-->         B003_APLIC,
NEW-->         B003_FXPRODUT,
NEW-->         B003_VLBASCUST,
NEW-->         B003_LIMFINAN,
NEW-->         B003_VLCOBPRO,
NEW-->         B003_TOTCOBPRO,
NEW-->         B003_PRECPRODT,
NEW-->         B003_PRECEMBAL,
NEW-->         B003_LIMPRODUC,
NEW-->         B003_LIMPRODUT,
NEW-->         B003_VFINAL,
NEW-->         B003_JUROS,
NEW-->         B003_CORRECAO,
NEW-->         B003_ADPRAGRO,
NEW-->         B003_COBERTU,
NEW-->         B003_VLCOBERT,
NEW-->         B003_CODCRS,
NEW-->         B003_DTACERTO,
NEW-->         B003_CODACERT,
NEW-->         B003_VLPRINCT_CAP9,
NEW-->         B003_VLPRINAC_PAG9,
NEW-->         B003_VLMUTORI_CAP9,
NEW-->         B003_VLMUTCOR_PAG9,
NEW-->         B003_VLPROCTA_CAP9,
NEW-->         B003_VLPROACE_PAG9,
NEW-->         B003_VLPRMUOR_CAP9,
NEW-->         B003_VLPRMUAC_PAG9,
NEW-->         B003_VLAUXIL_OPER9,
NEW-->         B003_JURPGORI_CAP9,
NEW-->         B003_JURPGAC_PAG9,
NEW-->         B003_CORRPGOR_CAP9,
NEW-->         B003_CORRPGAC_PAG9,
NEW-->         B003_PROAPGOR_CAP9,
NEW-->         B003_PROAPGAC_PAG9,
NEW-->         B003_TECNPGOR_CAP9,
NEW-->         B003_TECNPGAC_PAG9,
NEW-->         B003_PERIPGAC_PAG9,
NEW-->         B003_MEDIPGOR_CAP9,
NEW-->         B003_MEDIPGAC_PAG9,
NEW-->         B003_REMUPGOR_CAP9,
NEW-->         B003_REMUPGAC_PAG9,
NEW-->         B003_IOFPGORI_CAP9,
NEW-->         B003_IOFPGAC_PAG9,
NEW-->         B003_VLPRINCT_CAP8,
NEW-->         B003_VLPRINAC_PAG8,
NEW-->         B003_VLMUTORI_CAP8,
NEW-->         B003_VLMUTCOR_PAG8,
NEW-->         B003_VLPROCTA_CAP8,
NEW-->         B003_VLPROACE_PAG8,
NEW-->         B003_VLPRMUOR_CAP8,
NEW-->         B003_VLPRMUAC_PAG8,
NEW-->         B003_VLAUXIL_OPER8,
NEW-->         B003_JURPGORI_CAP8,
NEW-->         B003_JURPGAC_PAG8,
NEW-->         B003_CORRPGOR_CAP8,
NEW-->         B003_CORRPGAC_PAG8,
NEW-->         B003_PROAPGOR_CAP8,
NEW-->         B003_TECNPGOR_CAP8,
NEW-->         B003_TECNPGAC_PAG8,
NEW-->         B003_PERIPGOR_CAP8,
NEW-->         B003_PERIPGAC_PAG8,
NEW-->         B003_MEDIPGOR_CAP8,
NEW-->         B003_MEDIPGAC_PAG8,
NEW-->         B003_REMUPGOR_CAP8,
NEW-->         B003_REMUPGAC_PAG8,
NEW-->         B003_IOFPGORI_CAP8,
NEW-->         B003_IOFPGAC_PAG8
NEW-->       INTO
NEW-->         :B003-CONTROL,
NEW-->         :B003-NOMUTUAR1,
NEW-->         :B003-PESSOA1,
NEW-->         :B003-CGCPF1,
NEW-->         :B003-NOMUTUAR2,
NEW-->         :B003-PESSOA2,
NEW-->         :B003-CGCPF2,
NEW-->         :B003-NOMUTUAR3,
NEW-->         :B003-PESSOA3,
NEW-->         :B003-CGCPF3,
NEW-->         :B003-TPDEPEN,
NEW-->         :B003-MICREGIAO,
NEW-->         :B003-UF,
NEW-->         :B003-REGIAO,
NEW-->         :B003-EMERG,
NEW-->         :B003-ANOPROD,
NEW-->         :B003-VLPROD,
NEW-->         :B003-QTDMVR,
NEW-->         :B003-INDENIZ,
NEW-->         :B003-AGENAVAL,
NEW-->         :B003-CTAAVAL,
NEW-->         :B003-NOMAVAL,
NEW-->         :B003-PESAVAL,
NEW-->         :B003-CGCPFAVAL,
NEW-->         :B003-ESPBENS,
VERI-I         :B003-ORIGREC1,
VERI-I         :B003-IDENT1,
NEW-->         :B003-DATA1 :HR-NULL-C-DATA1,
NEW-->         :B003-ORIGREC2,
NEW-->         :B003-IDENT2,
NEW-->         :B003-DATA2 :HR-NULL-C-DATA2,
NEW-->         :B003-IDENT3,
NEW-->         :B003-DATA3 :HR-NULL-C-DATA3,
NEW-->         :B003-ORIGREC4,
NEW-->         :B003-IDENT4,
NEW-->         :B003-DATA4 :HR-NULL-C-DATA4,
NEW-->         :B003-IDENTREC,
NEW-->         :B003-PLREFINAM,
NEW-->         :B003-SPRGREFIN,
NEW-->         :B003-DTREFINAM :HR-NULL-C-DTREFINAM,
NEW-->         :B003-TXREFIN,
NEW-->         :B003-NCARTREF,
NEW-->         :B003-POSTO,
NEW-->         :B003-APLIC,
NEW-->         :B003-FXPRODUT,
NEW-->         :B003-VLBASCUST,
NEW-->         :B003-LIMFINAN,
NEW-->         :B003-VLCOBPRO,
NEW-->         :B003-TOTCOBPRO,
NEW-->         :B003-PRECPRODT,
NEW-->         :B003-PRECEMBAL,
NEW-->         :B003-LIMPRODUC,
NEW-->         :B003-VFINAL :HR-NULL-C-VFINAL,
NEW-->         :B003-JUROS,
NEW-->         :B003-CORRECAO,
NEW-->         :B003-ADPRAGRO,
NEW-->         :B003-COBERTU,
NEW-->         :B003-INCPAGRO,
NEW-->         :B003-VLCOBERT,
NEW-->         :B003-CODCRS,
NEW-->         :B003-DTACERTO :HR-NULL-C-DTACERTO,
NEW-->         :B003-CODACERT,
NEW-->         :B003-VLPRINCT-CAP9,
NEW-->         :B003-VLPRINAC-PAG9,
NEW-->         :B003-VLMUTORI-CAP9,
NEW-->         :B003-VLMUTCOR-PAG9,
NEW-->         :B003-VLPROCTA-CAP9,
NEW-->         :B003-VLPROACE-PAG9,
NEW-->         :B003-VLPRMUOR-CAP9,
NEW-->         :B003-VLPRMUAC-PAG9,
NEW-->         :B003-VLAUXIL-OPER9,
NEW-->         :B003-JURPGORI-CAP9,
NEW-->         :B003-JURPGAC-PAG9,
NEW-->         :B003-CORRPGAC-PAG9,
NEW-->         :B003-PROAPGOR-CAP9,
NEW-->         :B003-PROAPGAC-PAG9,
NEW-->         :B003-TECNPGOR-CAP9,
NEW-->         :B003-TECNPGAC-PAG9,
NEW-->         :B003-PERIPGOR-CAP9,
NEW-->         :B003-PERIPGAC-PAG9,
NEW-->         :B003-MEDIPGOR-CAP9,
NEW-->         :B003-MEDIPGAC-PAG9,
NEW-->         :B003-REMUPGOR-CAP9,
NEW-->         :B003-REMUPGAC-PAG9,
NEW-->         :B003-IOFPGORI-CAP9,
NEW-->         :B003-IOFPGAC-PAG9,
NEW-->         :B003-VLPRINCT-CAP8,
NEW-->         :B003-VLPRINAC-PAG8,
NEW-->         :B003-VLMUTORI-CAP8,
NEW-->         :B003-VLMUTCOR-PAG8,
NEW-->         :B003-VLPROCTA-CAP8,
NEW-->         :B003-VLPROACE-PAG8,
NEW-->         :B003-VLPRMUOR-CAP8,
NEW-->         :B003-VLPRMUAC-PAG8,
NEW-->         :B003-JURPGORI-CAP8,
NEW-->         :B003-JURPGAC-PAG8,
NEW-->         :B003-CORRPGOR-CAP8,
NEW-->         :B003-CORRPGAC-PAG8,
NEW-->         :B003-PROAPGOR-CAP8,
NEW-->         :B003-PROAPGAC-PAG8,
NEW-->         :B003-TECNPGOR-CAP8,
NEW-->         :B003-TECNPGAC-PAG8,
NEW-->         :B003-PERIPGOR-CAP8,
NEW-->         :B003-PERIPGAC-PAG8,
NEW-->         :B003-MEDIPGOR-CAP8,
NEW-->         :B003-MEDIPGAC-PAG8,
NEW-->         :B003-REMUPGOR-CAP8,
NEW-->         :B003-REMUPGAC-PAG8,
NEW-->         :B003-IOFPGORI-CAP8,
NEW-->         :B003-IOFPGAC-PAG8
NEW-->       FROM DB2PRD.RUCA_CAD_ROOT
NEW-->       WHERE
NEW-->         B003_CH_CONTR = :B003-CH-CONTR
NEW-->     END-EXEC
NEW-->     MOVE 'SELECT' TO HR-CURRENT-SQLSTMT
NEW-->     IF  SQLCODE NOT = ZEROS
NEW-->         GO TO HR-SELECT-RUCAB003-END.
NEW-->     MOVE B003-CH-CONTR TO HR03CAD1-C-CH-CONTR
NEW-->     MOVE B003-CONTROL TO HR03CAD2-C-CONTROL
NEW-->     MOVE B003-NOMUTUAR1 TO HR03CAD2-C-NOMUTUAR1
NEW-->     MOVE B003-PESSOA1 TO HR03CAD2-C-PESSOA1
NEW-->     MOVE B003-CGCPF1 TO HR03CAD2-C-CGCPF1
NEW-->     MOVE B003-NOMUTUAR2 TO HR03CAD2-C-NOMUTUAR2
NEW-->     MOVE B003-PESSOA2 TO HR03CAD2-C-PESSOA2
NEW-->     MOVE B003-CGCPF2 TO HR03CAD2-C-CGCPF2
NEW-->     MOVE B003-NOMUTUAR3 TO HR03CAD2-C-NOMUTUAR3
NEW-->     MOVE B003-PESSOA3 TO HR03CAD2-C-PESSOA3
NEW-->     MOVE B003-CGCPF3 TO HR03CAD2-C-CGCPF3
NEW-->     MOVE B003-NUMDEPEN TO HR03CAD2-C-NUMDEPEN
NEW-->     MOVE B003-TPDEPEN TO HR03CAD2-C-TPDEPEN
NEW-->     MOVE B003-MICREGIAO TO HR03CAD2-C-MICREGIAO
NEW-->     MOVE B003-UF TO HR03CAD2-C-UF
NEW-->     MOVE B003-REGIAO TO HR03CAD2-C-REGIAO
NEW-->     MOVE B003-EMERG TO HR03CAD2-C-EMERG
NEW-->     MOVE B003-ANOPROD TO HR03CAD2-C-ANOPROD
NEW-->     MOVE B003-VLPROD TO HR03CAD2-C-VLPROD
NEW-->     MOVE B003-INDENIZ TO HR03CAD2-C-INDENIZ
NEW-->     MOVE B003-AGENAVAL TO HR03CAD2-C-AGENAVAL
NEW-->     MOVE B003-CTAAVAL TO HR03CAD2-C-CTAAVAL
NEW-->     MOVE B003-NOMAVAL TO HR03CAD2-C-NOMAVAL
NEW-->     MOVE B003-PESAVAL TO HR03CAD2-C-PESAVAL
NEW-->     MOVE B003-CGCPFAVAL TO HR03CAD2-C-CGCPFAVAL
NEW-->     MOVE B003-ESPBENS  TO HR03CAD2-C-ESPBENS
VERI-I     MOVE B003-ORIGREC1 TO HR03CAD2-C-ORIGREC1
VERI-I     MOVE B003-IDENT1   TO HR03CAD2-C-IDENT1
NEW-->     IF  HR-NULL-C-DATA1 IS NOT NEGATIVE
NEW-->         MOVE B003-DATA1 TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD2-C-DATA1
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD2-C-DATA1.
NEW-->     MOVE B003-ORIGREC2 TO HR03CAD2-C-ORIGREC2
NEW-->     MOVE B003-IDENT2 TO HR03CAD2-C-IDENT2
NEW-->     IF  HR-NULL-C-DATA2 IS NOT NEGATIVE
NEW-->         MOVE B003-DATA2 TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD2-C-DATA2
NEW-->         MOVE ZEROS TO HR03CAD2-C-DATA2.
NEW-->     MOVE B003-ORIGREC3 TO HR03CAD2-C-ORIGREC3
NEW-->     MOVE B003-IDENT3 TO HR03CAD2-C-IDENT3
NEW-->     IF  HR-NULL-C-DATA3 IS NOT NEGATIVE
NEW-->         MOVE B003-DATA3 TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD2-C-DATA3
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD2-C-DATA3.
NEW-->     MOVE B003-ORIGREC4 TO HR03CAD2-C-ORIGREC4
NEW-->     MOVE B003-IDENT4 TO HR03CAD2-C-IDENT4
NEW-->     IF  HR-NULL-C-DATA4 IS NOT NEGATIVE
NEW-->         MOVE B003-DATA4 TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD2-C-DATA4
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD2-C-DATA4.
NEW-->     MOVE B003-IDENTREC TO HR03CAD2-C-IDENTREC
NEW-->     MOVE B003-PLREFINAM TO HR03CAD2-C-PLREFINAM
NEW-->     MOVE B003-SPRGREFIN TO HR03CAD2-C-SPRGREFIN
NEW-->     IF  HR-NULL-C-DTREFINAM IS NOT NEGATIVE
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD2-C-DTREFINAM
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD2-C-DTREFINAM.
NEW-->     MOVE B003-TXREFIN TO HR03CAD2-C-TXREFIN
NEW-->     MOVE B003-NCARTREF TO HR03CAD2-C-NCARTREF
NEW-->     MOVE B003-POSTO TO HR03CAD2-C-POSTO
NEW-->     MOVE B003-APLIC TO HR03CAD2-C-APLIC
NEW-->     MOVE B003-FXPRODUT TO HR03CAD2-C-FXPRODUT
NEW-->     MOVE B003-VLBASCUST TO HR03CAD2-C-VLBASCUST
NEW-->     MOVE B003-LIMFINAN TO HR03CAD2-C-LIMFINAN
NEW-->     MOVE B003-VLCOBPRO TO HR03CAD2-C-VLCOBPRO
NEW-->     MOVE B003-TOTCOBPRO TO HR03CAD2-C-TOTCOBPRO
NEW-->     MOVE B003-PRECPRODT TO HR03CAD2-C-PRECPRODT
NEW-->     MOVE B003-PRECEMBAL TO HR03CAD2-C-PRECEMBAL
NEW-->     MOVE B003-LIMPRODUC TO HR03CAD2-C-LIMPRODUC
NEW-->     MOVE B003-LIMPRODUT TO HR03CAD2-C-LIMPRODUT
NEW-->     IF  HR-NULL-C-VFINAL IS NOT NEGATIVE
NEW-->         MOVE B003-VFINAL TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD2-C-VFINAL
NEW-->         MOVE ZEROS TO HR03CAD2-C-VFINAL.
NEW-->     MOVE B003-JUROS TO HR03CAD2-C-JUROS
NEW-->     MOVE B003-CORRECAO TO HR03CAD2-C-CORRECAO
NEW-->     MOVE B003-ADPRAGRO TO HR03CAD2-C-ADPRAGRO
NEW-->     MOVE B003-COBERTU TO HR03CAD2-C-COBERTU
NEW-->     MOVE B003-INCPAGRO TO HR03CAD2-C-INCPAGRO
NEW-->     MOVE B003-VLCOBERT TO HR03CAD2-C-VLCOBERT
NEW-->     MOVE B003-CODCRS TO HR03CAD2-C-CODCRS
NEW-->     IF  HR-NULL-C-DTACERTO IS NOT NEGATIVE
NEW-->         MOVE B003-DTACERTO TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD2-C-DTACERTO
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD2-C-DTACERTO.
NEW-->     MOVE B003-CODACERT TO HR03CAD2-C-CODACERT
NEW-->     MOVE B003-VLPRINCT-CAP9 TO HR03CAD2-C-VLPRINCT-CAP999
NEW-->     MOVE B003-VLPRINAC-PAG9 TO HR03CAD2-C-VLPRINAC-PAG999
NEW-->     MOVE B003-VLMUTORI-CAP9 TO HR03CAD2-C-VLMUTORI-CAP999
NEW-->     MOVE B003-VLMUTCOR-PAG9 TO HR03CAD2-C-VLMUTCOR-PAG999
NEW-->     MOVE B003-VLPROCTA-CAP9 TO HR03CAD2-C-VLPROCTA-CAP999
NEW-->     MOVE B003-VLPROACE-PAG9 TO HR03CAD2-C-VLPROACE-PAG999
NEW-->     MOVE B003-VLPRMUAC-PAG9 TO HR03CAD2-C-VLPRMUAC-PAG999
NEW-->     MOVE B003-VLAUXIL-OPER9 TO HR03CAD2-C-VLAUXIL-OPER999
NEW-->     MOVE B003-JURPGORI-CAP9 TO HR03CAD2-C-JURPGORI-CAP999
NEW-->     MOVE B003-JURPGAC-PAG9 TO HR03CAD2-C-JURPGAC-PAG999
NEW-->     MOVE B003-CORRPGOR-CAP9 TO HR03CAD2-C-CORRPGOR-CAP999
NEW-->     MOVE B003-CORRPGAC-PAG9 TO HR03CAD2-C-CORRPGAC-PAG999
NEW-->     MOVE B003-PROAPGOR-CAP9 TO HR03CAD2-C-PROAPGOR-CAP999
NEW-->     MOVE B003-PROAPGAC-PAG9 TO HR03CAD2-C-PROAPGAC-PAG999
NEW-->     MOVE B003-TECNPGOR-CAP9 TO HR03CAD2-C-TECNPGOR-CAP999
NEW-->     MOVE B003-TECNPGAC-PAG9 TO HR03CAD2-C-TECNPGAC-PAG999
NEW-->     MOVE B003-PERIPGOR-CAP9 TO HR03CAD2-C-PERIPGOR-CAP999
NEW-->     MOVE B003-PERIPGAC-PAG9 TO HR03CAD2-C-PERIPGAC-PAG999
NEW-->     MOVE B003-MEDIPGOR-CAP9 TO HR03CAD2-C-MEDIPGOR-CAP999
NEW-->     MOVE B003-MEDIPGAC-PAG9 TO HR03CAD2-C-MEDIPGAC-PAG999
NEW-->     MOVE B003-REMUPGOR-CAP9 TO HR03CAD2-C-REMUPGOR-CAP999
NEW-->     MOVE B003-REMUPGAC-PAG9 TO HR03CAD2-C-REMUPGAC-PAG999
NEW-->     MOVE B003-IOFPGORI-CAP9 TO HR03CAD2-C-IOFPGORI-CAP999
NEW-->     MOVE B003-IOFPGAC-PAG9 TO HR03CAD2-C-IOFPGAC-PAG999
NEW-->     MOVE B003-VLPRINCT-CAP8 TO HR03CAD2-C-VLPRINCT-CAP998
NEW-->     MOVE B003-VLPRINAC-PAG8 TO HR03CAD2-C-VLPRINAC-PAG998
NEW-->     MOVE B003-VLMUTORI-CAP8 TO HR03CAD2-C-VLMUTORI-CAP998
NEW-->     MOVE B003-VLPROCTA-CAP8 TO HR03CAD2-C-VLPROCTA-CAP998
NEW-->     MOVE B003-VLPROACE-PAG8 TO HR03CAD2-C-VLPROACE-PAG998
NEW-->     MOVE B003-VLPRMUOR-CAP8 TO HR03CAD2-C-VLPRMUOR-CAP998
NEW-->     MOVE B003-VLPRMUAC-PAG8 TO HR03CAD2-C-VLPRMUAC-PAG998
NEW-->     MOVE B003-VLAUXIL-OPER8 TO HR03CAD2-C-VLAUXIL-OPER998
NEW-->     MOVE B003-JURPGORI-CAP8 TO HR03CAD2-C-JURPGORI-CAP998
NEW-->     MOVE B003-JURPGAC-PAG8 TO HR03CAD2-C-JURPGAC-PAG998
NEW-->     MOVE B003-CORRPGOR-CAP8 TO HR03CAD2-C-CORRPGOR-CAP998
NEW-->     MOVE B003-CORRPGAC-PAG8 TO HR03CAD2-C-CORRPGAC-PAG998
NEW-->     MOVE B003-PROAPGOR-CAP8 TO HR03CAD2-C-PROAPGOR-CAP998
NEW-->     MOVE B003-PROAPGAC-PAG8 TO HR03CAD2-C-PROAPGAC-PAG998
NEW-->     MOVE B003-TECNPGOR-CAP8 TO HR03CAD2-C-TECNPGOR-CAP998
NEW-->     MOVE B003-TECNPGAC-PAG8 TO HR03CAD2-C-TECNPGAC-PAG998
NEW-->     MOVE B003-PERIPGOR-CAP8 TO HR03CAD2-C-PERIPGOR-CAP998
NEW-->     MOVE B003-PERIPGAC-PAG8 TO HR03CAD2-C-PERIPGAC-PAG998
NEW-->     MOVE B003-MEDIPGOR-CAP8 TO HR03CAD2-C-MEDIPGOR-CAP998
NEW-->     MOVE B003-MEDIPGAC-PAG8 TO HR03CAD2-C-MEDIPGAC-PAG998
NEW-->     MOVE B003-REMUPGOR-CAP8 TO HR03CAD2-C-REMUPGOR-CAP998
NEW-->     MOVE B003-REMUPGAC-PAG8 TO HR03CAD2-C-REMUPGAC-PAG998
NEW-->     MOVE B003-IOFPGORI-CAP8 TO HR03CAD2-C-IOFPGORI-CAP998
NEW-->     MOVE B003-IOFPGAC-PAG8 TO HR03CAD2-C-IOFPGAC-PAG998
NEW--> HR-SELECT-RUCAB003-END. EXIT.
NEW-->*
NEW--> HR-SELECT-RUCAB004 SECTION.
NEW-->     IF  NOT HR-PREVIOUS-OK
NEW-->         GO TO HR-SELECT-RUCAB004-END.
NEW-->     MOVE 'N' TO HR-PERFORM-STATUS
NEW-->     EXEC SQL
NEW-->       SELECT
NEW-->         B004_ESPECIE,
NEW-->         B004_CONTROLE,
NEW-->         B004_DTEVENTO,
NEW-->         B004_CODLANC,
NEW-->         B004_DTCONTAB,
NEW-->         B004_DTACERT,
NEW-->         B004_IDENTDC,
NEW-->         B004_DEBCRED,
NEW-->         B004_VLPRIORI,
NEW-->         B004_VLPRICOR,
NEW-->         B004_VLMUTORI,
NEW-->         B004_VLMUTCOR,
NEW-->         B004_VLPROORI,
NEW-->         B004_VLPRMUORI,
NEW-->         B004_VLPRMUCOR,
NEW-->         B004_VLAUXIL,
NEW-->         B004_JURORIG,
NEW-->         B004_JURCORRI,
NEW-->         B004_CORRORIG,
NEW-->         B004_CORRCORRI,
NEW-->         B004_PROAORIG,
NEW-->         B004_PROACORR,
NEW-->         B004_ASSIORIG,
NEW-->         B004_ASSICORR,
NEW-->         B004_LAUDORIG,
NEW-->         B004_LAUDCORR,
NEW-->         B004_MEDIORIG,
NEW-->         B004_MEDICORR,
NEW-->         B004_REMUORIG,
NEW-->         B004_REMUCORR,
NEW-->         B004_IOFORIG,
NEW-->         B004_IOFCORR,
NEW-->         B004_AGDEBCRED,
NEW-->         B004_RZDEBCRED,
NEW-->         B004_DTMORA,
NEW-->         B004_LANCMORA,
NEW-->         B004_DBCRMORA,
NEW-->         B004_NUMMORA,
NEW-->         B004_CODCRS,
NEW-->         B004_DTACERTO,
NEW-->         B004_CODACERT
NEW-->       INTO
NEW-->         :B004-ESPECIE,
NEW-->         :B004-CONTROLE,
NEW-->         :B004-DTEVENTO :HR-NULL-D-DTEVENTO,
NEW-->         :B004-CODLANC,
NEW-->         :B004-DTCONTAB :HR-NULL-D-DTCONTAB,
NEW-->         :B004-DTACERT :HR-NULL-D-DTACERT,
NEW-->         :B004-IDENTDC,
NEW-->         :B004-DEBCRED,
NEW-->         :B004-VLPRIORI,
NEW-->         :B004-VLPRICOR,
NEW-->         :B004-VLMUTORI,
NEW-->         :B004-VLMUTCOR,
NEW-->         :B004-VLPROORI,
NEW-->         :B004-VLPRMUORI,
NEW-->         :B004-VLPRMUCOR,
NEW-->         :B004-VLAUXIL,
NEW-->         :B004-JURORIG,
NEW-->         :B004-JURCORRI,
NEW-->         :B004-CORRORIG,
NEW-->         :B004-CORRCORRI,
NEW-->         :B004-PROAORIG,
NEW-->         :B004-PROACORR,
NEW-->         :B004-ASSIORIG,
NEW-->         :B004-ASSICORR,
NEW-->         :B004-LAUDORIG,
NEW-->         :B004-LAUDCORR,
NEW-->         :B004-MEDIORIG,
NEW-->         :B004-MEDICORR,
NEW-->         :B004-REMUORIG,
NEW-->         :B004-REMUCORR,
NEW-->         :B004-IOFORIG,
NEW-->         :B004-IOFCORR,
NEW-->         :B004-AGDEBCRED,
NEW-->         :B004-RZDEBCRED,
NEW-->         :B004-DTMORA,
NEW-->         :B004-LANCMORA,
NEW-->         :B004-DBCRMORA,
NEW-->         :B004-NUMMORA,
NEW-->         :B004-CODCRS,
NEW-->         :B004-DTACERTO :HR-NULL-D-DTACERTO,
NEW-->         :B004-CODACERT
NEW-->       FROM DB2PRD.RUCA_CAD_DEP
NEW-->       WHERE
NEW-->         B004_CH_CONTR = :B004-CH-CONTR
NEW-->         AND
NEW-->         B004_CH_TIPO = :B004-CH-TIPO
NEW-->         AND
NEW-->         B004_CH_NUMERO = :B004-CH-NUMERO
NEW-->     END-EXEC
NEW-->     MOVE 'SELECT' TO HR-CURRENT-SQLSTMT
NEW-->     PERFORM HR-CHECK-SQLCODE
NEW-->     IF  SQLCODE NOT = ZEROS
NEW-->         GO TO HR-SELECT-RUCAB004-END.
NEW-->     MOVE B004-CH-CONTR TO HR03CAD1-C-CH-CONTR
NEW-->     MOVE B004-CH-TIPO TO HR03CAD3-D-CH-TIPO
NEW-->     MOVE B004-ESPECIE TO HR03CAD3-D-ESPECIE
NEW-->     MOVE B004-CONTROLE TO HR03CAD3-D-CONTROLE
NEW-->     IF  HR-NULL-D-DTEVENTO IS NOT NEGATIVE
NEW-->         MOVE B004-DTEVENTO TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD3-D-DTEVENTO
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD3-D-DTEVENTO.
NEW-->     MOVE B004-CODLANC TO HR03CAD3-D-CODLANC
NEW-->     IF  HR-NULL-D-DTCONTAB IS NOT NEGATIVE
NEW-->         MOVE B004-DTCONTAB TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD3-D-DTCONTAB
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD3-D-DTCONTAB.
NEW-->     IF  HR-NULL-D-DTACERT IS NOT NEGATIVE
NEW-->         MOVE B004-DTACERT TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD3-D-DTACERT
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD3-D-DTACERT.
NEW-->     MOVE B004-DEBCRED TO HR03CAD3-D-DEBCRED
NEW-->     MOVE B004-VLPRIORI TO HR03CAD3-D-VLPRIORI
NEW-->     MOVE B004-VLPRICOR TO HR03CAD3-D-VLPRICOR
NEW-->     MOVE B004-VLMUTORI TO HR03CAD3-D-VLMUTORI
NEW-->     MOVE B004-VLMUTCOR TO HR03CAD3-D-VLMUTCOR
NEW-->     MOVE B004-VLPROORI TO HR03CAD3-D-VLPROORI
NEW-->     MOVE B004-VLPROCOR TO HR03CAD3-D-VLPROCOR
NEW-->     MOVE B004-VLPRMUORI TO HR03CAD3-D-VLPRMUORI
NEW-->     MOVE B004-VLPRMUCOR TO HR03CAD3-D-VLPRMUCOR
NEW-->     MOVE B004-VLAUXIL TO HR03CAD3-D-VLAUXIL
NEW-->     MOVE B004-JURORIG TO HR03CAD3-D-JURORIG
NEW-->     MOVE B004-JURCORRI TO HR03CAD3-D-JURCORRI
NEW-->     MOVE B004-CORRORIG TO HR03CAD3-D-CORRORIG
NEW-->     MOVE B004-CORRCORRI TO HR03CAD3-D-CORRCORRI
NEW-->     MOVE B004-PROAORIG TO HR03CAD3-D-PROAORIG
NEW-->     MOVE B004-PROACORR TO HR03CAD3-D-PROACORR
NEW-->     MOVE B004-ASSIORIG TO HR03CAD3-D-ASSIORIG
NEW-->     MOVE B004-ASSICORR TO HR03CAD3-D-ASSICORR
NEW-->     MOVE B004-LAUDORIG TO HR03CAD3-D-LAUDORIG
NEW-->     MOVE B004-LAUDCORR TO HR03CAD3-D-LAUDCORR
NEW-->     MOVE B004-MEDIORIG TO HR03CAD3-D-MEDIORIG
NEW-->     MOVE B004-REMUORIG TO HR03CAD3-D-REMUORIG
NEW-->     MOVE B004-REMUCORR TO HR03CAD3-D-REMUCORR
NEW-->     MOVE B004-IOFORIG TO HR03CAD3-D-IOFORIG
NEW-->     MOVE B004-IOFCORR TO HR03CAD3-D-IOFCORR
NEW-->     MOVE B004-AGDEBCRED TO HR03CAD3-D-AGDEBCRED
NEW-->     MOVE B004-RZDEBCRED TO HR03CAD3-D-RZDEBCRED
NEW-->     MOVE B004-CTDEBCRED TO HR03CAD3-D-CTDEBCRED
NEW-->     MOVE B004-DTMORA TO HR03CAD3-D-DTMORA
NEW-->     MOVE B004-LANCMORA TO HR03CAD3-D-LANCMORA
NEW-->     MOVE B004-DBCRMORA TO HR03CAD3-D-DBCRMORA
NEW-->     MOVE B004-NUMMORA TO HR03CAD3-D-NUMMORA
NEW-->     MOVE B004-CODCRS TO HR03CAD3-D-CODCRS
NEW-->     IF  HR-NULL-D-DTACERTO IS NOT NEGATIVE
NEW-->         MOVE B004-DTACERTO TO HR-SQL-DATC
NEW-->         PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->         MOVE HR-DATC-YMD TO HR03CAD3-D-DTACERTO
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD3-D-DTACERTO.
NEW-->     MOVE B004-CODACERT TO HR03CAD3-D-CODACERT
NEW-->     MOVE 'Y' TO HR-PERFORM-STATUS.
NEW--> HR-SELECT-RUCAB004-END. EXIT.
NEW--> HR-SELECT-RUFIV002 SECTION.
NEW-->     IF  NOT HR-PREVIOUS-OK
NEW-->         GO TO HR-SELECT-RUFIV002-END.
NEW-->     MOVE 'N' TO HR-PERFORM-STATUS
NEW-->     EXEC SQL
NEW-->       SELECT
NEW-->         CCTA_CORR,
NEW-->         CCTA_CTBIL_VINC,
NEW-->         CTPO_PROTR_RURAL,
NEW-->         CJUNC_DEPDC,
NEW-->         CCDULA,
NEW-->         CMUN_RURAL,
NEW-->         CCTRO_FSCAL_RURAL,
NEW-->         MAREA_TOT,
NEW-->         IPPRIE,
NEW-->         CGARTD,
NEW-->         CESPCE_TITLO_GARNT,
NEW-->         CTPO_GARNT,
NEW-->         QTOT_GARNT,
NEW-->         VTOT_GARNT,
NEW-->         CORIGE_REC,
NEW-->         CREFT_BACEN,
NEW-->         CAUTRZ_OPER,
NEW-->         CTPO_CONDC_FSCAL,
NEW-->         CATVDD_RURAL,
NEW-->         CFNALD_RURAL,
NEW-->         CPRODT_RURAL,
NEW-->         QUND_FINCD_RURAL,
NEW-->         CUND_MEDD_FINCD,
NEW-->         QUND_PROD_PROVV,
NEW-->         CUND_MEDD_PROVV,
NEW-->         DINIC_SAFRA,
NEW-->         DFIM_SAFRA,
NEW-->         VFINAN,
NEW-->         VREC_PPRIO,
NEW-->         CINCID_PROAGRO,
NEW-->         DVCTO_ORIGN_CONTR,
NEW-->         DCDULA,
NEW-->         DVCTO_CONTR,
NEW-->         DINCL,
NEW-->         CCLASF_PRODT_RURAL,
NEW-->         CTPO_EMBAL,
NEW-->         QCAPAC_PROD,
NEW-->         VEGF,
               VORCAM
POL128       , CSEGDR_CREDT_RURAL
ACT115       , DIMPRE_CONTR_RURAL
PW9931       , CREFT_RECOR
BR0118       , CDECLR_AGCLT_FAMLR
BR0118       , DVALDD_DECLR_FAMLR
BR0118       , CGRP_AGCLT_FAMLR
ECR          , CFORMA_LIQDC
NEW-->       INTO
NEW-->         :CCTA-CORR,
NEW-->         :CCTA-CTBIL-VINC :HR-NULL-C-CTAVINC,
NEW-->         :CTPO-PROTR-RURAL :HR-NULL-C-PRODUTOR,
NEW-->         :MESUV001.CJUNC-DEPDC,
NEW-->         :CCDULA,
NEW-->         :CMUN-RURAL :HR-NULL-C-MUNIC,
NEW-->         :CCTRO-FSCAL-RURAL :HR-NULL-C-CENTRO,
NEW-->         :MAREA-TOT,
NEW-->         :IPPRIE,
NEW-->         :CGARTD :HR-NULL-C-FIANCA,
NEW-->         :CTPO-GARNT :HR-NULL-C-GARANTIA,
NEW-->         :QTOT-GARNT,
NEW-->         :VTOT-GARNT,
NEW-->         :RUFIV002.CORIGE-REC :HR-NULL-C-ORIGREC1,
NEW-->         :CCONDC-ESPCL-RURAL,
NEW-->         :CREFT-BACEN,
NEW-->         :CAUTRZ-OPER :HR-NULL-C-NUMAUTOR,
NEW-->         :CTPO-CONDC-FSCAL :HR-NULL-C-FISCALIZ,
NEW-->         :RUFIV002.CATVDD-RURAL :HR-NULL-C-ATIVID,
NEW-->         :RUFIV002.CFNALD-RURAL :HR-NULL-C-FINALID,
NEW-->         :CPRODT-RURAL :HR-NULL-C-EMPREEND,
NEW-->         :QUND-FINCD-RURAL,
NEW-->         :CUND-MEDD-FINCD :HR-NULL-C-UNIDADE1,
NEW-->         :QUND-PROD-PROVV,
NEW-->         :CUND-MEDD-PROVV :HR-NULL-C-UNIDADE2,
NEW-->         :DINIC-SAFRA :HR-NULL-C-SAFRA-INI,
NEW-->         :DFIM-SAFRA :HR-NULL-C-SAFRA-FIM,
NEW-->         :VFINAN,
NEW-->         :VREC-PPRIO,
NEW-->         :CINCID-PROAGRO,
NEW-->         :DVCTO-ORIGN-CONTR,
NEW-->         :DVCTO-CONTR,
NEW-->         :RUFIV002.DINCL,
NEW-->         :CCLASF-PRODT-RURAL :HR-NULL-C-CLASPRODT,
NEW-->         :CTPO-EMBAL :HR-NULL-C-CODEMBAL,
NEW-->         :CCATEG-MUTUA :HR-NULL-C-CATEGMUT,
NEW-->         :QCAPAC-PROD,
NEW-->         :VEGF :HR-NULL-C-EGFOUTROS,
               :VORCAM
POL128       , :CSEGDR-CREDT-RURAL
ACT115       , :DIMPRE-CONTR-RURAL
PW9931       , :CREFT-RECOR
BR0118       , :CDECLR-AGCLT-FAMLR :WRK-NULL-C-DECLR-AGCLT
BR0118       , :DVALDD-DECLR-FAMLR :WRK-NULL-C-DVALDD-DECLR
BR0118       , :RUFIV002.CGRP-AGCLT-FAMLR   :WRK-NULL-C-CGRP-AGCLT
BR0118       , :RUFIV002.CFORMA-LIQDC  :WRK-NULL-CFORMA-LIQDC
VERI-I       FROM DB2PRD.V01CONTRATO_RURAL
NEW-->       WHERE
NEW-->         CCONTR = :RUFIV002.CCONTR AND
ACT006        (CCTRO_CUSTO = :RUFIV002.CCTRO-CUSTO OR
BS0214         CCTRO_CUSTO = :WRK-CCTRO-CUSTO-AUX)
NEW-->     END-EXEC
NEW-->     PERFORM HR-CHECK-SQLCODE
NEW-->     IF  SQLCODE NOT = ZEROS
NEW-->         GO TO HR-SELECT-RUFIV002-END.
NEW-->     MOVE CCONTR OF RUFIV002 TO HR03CAD1-C-CH-CONTR
NEW-->     MOVE CCTA-CORR TO HR03CAD1-C-CTACORR
NEW-->     IF  HR-NULL-C-CTAVINC IS NOT NEGATIVE
NEW-->         MOVE CCTA-CTBIL-VINC TO HR03CAD1-C-CTAVINC
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-CTAVINC.
NEW-->     IF  HR-NULL-C-PRODUTOR IS NOT NEGATIVE
NEW-->         MOVE CTPO-PROTR-RURAL TO HR03CAD1-C-PRODUTOR
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-PRODUTOR.
NEW-->     MOVE CJUNC-DEPDC OF MESUV001 TO HR03CAD1-C-AGENCIA
NEW-->     MOVE CCDULA TO HR03CAD1-C-NUMEMPR
NEW-->     IF  HR-NULL-C-MUNIC IS NOT NEGATIVE
NEW-->         MOVE CMUN-RURAL TO HR03CAD1-C-MUNIC
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-MUNIC.
NEW-->     IF  HR-NULL-C-CENTRO IS NOT NEGATIVE
NEW-->         MOVE CCTRO-FSCAL-RURAL TO HR03CAD1-C-CENTRO
NEW-->         MOVE ZEROS TO HR03CAD1-C-CENTRO.
NEW-->     MOVE MAREA-TOT TO HR03CAD1-C-AREATOT
NEW-->     MOVE IPPRIE TO HR03CAD1-C-NOMIMOVEL
NEW-->     IF  HR-NULL-C-FIANCA IS NOT NEGATIVE
NEW-->         MOVE CGARTD TO HR03CAD1-C-FIANCA
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-FIANCA
                             CGARTD OF RUFIV002.
POL128     MOVE CSEGDR-CREDT-RURAL TO HR03CAD1-C-SEGURADORA
NEW-->     IF  HR-NULL-C-TITULO IS NOT NEGATIVE
NEW-->         MOVE CESPCE-TITLO-GARNT TO HR03CAD1-C-TITULO
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-TITULO.
NEW-->     IF  HR-NULL-C-GARANTIA IS NOT NEGATIVE
NEW-->         MOVE CTPO-GARNT TO HR03CAD1-C-GARANTIA
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-GARANTIA.
NEW-->     MOVE QTOT-GARNT TO HR03CAD1-C-QTDBENS
NEW-->     MOVE VTOT-GARNT TO HR03CAD1-C-VLBENS
NEW-->     MOVE CREFT-BACEN TO HR03CAD1-C-NUMRECOR
NEW-->     IF  HR-NULL-C-NUMAUTOR IS NOT NEGATIVE
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-NUMAUTOR.
NEW-->     IF  HR-NULL-C-FISCALIZ IS NOT NEGATIVE
NEW-->         MOVE CTPO-CONDC-FSCAL TO HR03CAD1-C-FISCALIZ
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-FISCALIZ.
NEW-->     IF  HR-NULL-C-ATIVID IS NOT NEGATIVE
ECR            MOVE CATVDD-RURAL OF RUFIV002 TO HR03CAD1-C-ATIVID
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-ATIVID.
NEW-->     IF  HR-NULL-C-FINALID IS NOT NEGATIVE
ECR            MOVE CFNALD-RURAL OF RUFIV002 TO HR03CAD1-C-FINALID
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-FINALID.
NEW-->     IF  HR-NULL-C-EMPREEND IS NOT NEGATIVE
NEW-->         MOVE CPRODT-RURAL TO HR03CAD1-C-EMPREEND
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-EMPREEND.
NEW-->     MOVE QUND-FINCD-RURAL TO HR03CAD1-C-QTDFINAN
NEW-->     IF  HR-NULL-C-UNIDADE1 IS NOT NEGATIVE
NEW-->         MOVE CUND-MEDD-FINCD TO HR03CAD1-C-UNIDADE1
NEW-->         MOVE ZEROS TO HR03CAD1-C-UNIDADE1.
NEW-->     MOVE QUND-PROD-PROVV TO HR03CAD1-C-PRODPROV
NEW-->     IF  HR-NULL-C-UNIDADE2 IS NOT NEGATIVE
NEW-->         MOVE CUND-MEDD-PROVV TO HR03CAD1-C-UNIDADE2
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-UNIDADE2.
NEW-->     IF  HR-NULL-C-SAFRA-INI IS NOT NEGATIVE
NEW-->         MOVE DINIC-SAFRA TO HR03CAD1-C-SAFRA-INI
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-SAFRA-INI.
NEW-->     IF  HR-NULL-C-SAFRA-FIM IS NOT NEGATIVE
NEW-->         MOVE DFIM-SAFRA TO HR03CAD1-C-SAFRA-FIM
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-SAFRA-FIM.
NEW-->     MOVE VFINAN TO HR03CAD1-C-TOTFINAN
NEW-->     MOVE VREC-PPRIO TO HR03CAD1-C-RECPROPR
NEW-->     MOVE CINCID-PROAGRO TO HR03CAD1-C-CODINCID
NEW-->     MOVE DVCTO-ORIGN-CONTR TO HR-SQL-DATC
NEW-->     PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->     MOVE HR-DATC-YMD TO HR03CAD1-C-DTVENCTO
NEW-->     MOVE DCDULA TO HR-SQL-DATC
NEW-->     MOVE HR-DATC-YMD TO HR03CAD1-C-DTCEDULA
NEW-->     MOVE DVCTO-CONTR TO HR-SQL-DATC
NEW-->     PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->     MOVE HR-DATC-YMD TO HR03CAD1-C-DTOPER
ECR        MOVE DINCL OF RUFIV002 TO HR-SQL-DATC
NEW-->     PERFORM HR-CVDATC-SQL-TO-YMD
NEW-->     MOVE HR-DATC-YMD TO HR03CAD1-C-DTREGIST
NEW-->     IF  HR-NULL-C-CLASPRODT IS NOT NEGATIVE
NEW-->         MOVE CCLASF-PRODT-RURAL TO HR03CAD1-C-CLASPRODT
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-CLASPRODT.
NEW-->     IF  HR-NULL-C-CODEMBAL IS NOT NEGATIVE
NEW-->         MOVE CTPO-EMBAL TO HR03CAD1-C-CODEMBAL
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-CODEMBAL.
NEW-->     IF  HR-NULL-C-CATEGMUT IS NOT NEGATIVE
NEW-->         MOVE CCATEG-MUTUA TO HR03CAD1-C-CATEGMUT
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-CATEGMUT.
NEW-->     MOVE QCAPAC-PROD TO HR03CAD1-C-CAPPRODUC
NEW-->     IF  HR-NULL-C-EGFOUTROS IS NOT NEGATIVE
NEW-->     ELSE
NEW-->         MOVE ZEROS TO HR03CAD1-C-EGFOUTROS.

ACT115     IF  DIMPRE-CONTR-RURAL  EQUAL  '01.01.0001'
ACT115         MOVE DCDULA TO DIMPRE-CONTR-RURAL
ACT115     END-IF.

BR0118     IF  WRK-NULL-C-DECLR-AGCLT    IS NEGATIVE
BR0118         MOVE ZEROS TO CDECLR-AGCLT-FAMLR
BR0118     END-IF

BR0118     IF  WRK-NULL-C-DVALDD-DECLR   IS NEGATIVE
BR0118         MOVE ZEROS TO DVALDD-DECLR-FAMLR
BR0118     END-IF

BR0118     IF  WRK-NULL-C-CGRP-AGCLT     IS NEGATIVE
BR0118         MOVE ZEROS TO CGRP-AGCLT-FAMLR OF RUFIV002
BR0118     END-IF

ECR        IF  WRK-NULL-CFORMA-LIQDC     IS NEGATIVE
ICR            MOVE ZEROS TO CFORMA-LIQDC     OF RUFIV002
ECR
ECR        IF CFORMA-LIQDC OF RUFIV002    EQUAL 3
ECR        OR CCTA-CORR    OF RUFIV002    EQUAL 999999
ECR           MOVE 'BOLETO'               TO WRK-OUT-FORMA-PGTO
ECR        ELSE
ECR           MOVE 'DEBITO EM CONTA'      TO WRK-OUT-FORMA-PGTO
ECR        END-IF.
ECR
ECR
NEW-->     MOVE 'Y' TO HR-PERFORM-STATUS.
NEW--> HR-SELECT-RUFIV002-END. EXIT.
NEW-->*
NEW--> HR-UPDATE-RUCAB003 SECTION.
NEW-->     MOVE 'N' TO HR-PERFORM-STATUS
NEW-->     MOVE HR03CAD2-C-CONTROL TO B003-CONTROL
NEW-->     MOVE HR03CAD2-C-NOMUTUAR1 TO B003-NOMUTUAR1
NEW-->     MOVE HR03CAD2-C-PESSOA1 TO B003-PESSOA1
NEW-->     MOVE HR03CAD2-C-CGCPF1 TO B003-CGCPF1
NEW-->     MOVE HR03CAD2-C-NOMUTUAR2 TO B003-NOMUTUAR2
NEW-->     MOVE HR03CAD2-C-PESSOA2 TO B003-PESSOA2
NEW-->     MOVE HR03CAD2-C-CGCPF2 TO B003-CGCPF2
NEW-->     MOVE HR03CAD2-C-PESSOA3 TO B003-PESSOA3
NEW-->     MOVE HR03CAD2-C-CGCPF3 TO B003-CGCPF3
NEW-->     MOVE HR03CAD2-C-NUMDEPEN TO B003-NUMDEPEN
NEW-->     MOVE HR03CAD2-C-TPDEPEN TO B003-TPDEPEN
NEW-->     MOVE HR03CAD2-C-MICREGIAO TO B003-MICREGIAO
NEW-->     MOVE HR03CAD2-C-UF TO B003-UF
NEW-->     MOVE HR03CAD2-C-REGIAO TO B003-REGIAO
NEW-->     MOVE HR03CAD2-C-EMERG TO B003-EMERG
NEW-->     MOVE HR03CAD2-C-ANOPROD TO B003-ANOPROD
NEW-->     MOVE HR03CAD2-C-VLPROD TO B003-VLPROD
NEW-->     MOVE HR03CAD2-C-QTDMVR TO B003-QTDMVR
NEW-->     MOVE HR03CAD2-C-INDENIZ TO B003-INDENIZ
NEW-->     MOVE HR03CAD2-C-AGENAVAL TO B003-AGENAVAL
NEW-->     MOVE HR03CAD2-C-CTAAVAL TO B003-CTAAVAL
NEW-->     MOVE HR03CAD2-C-NOMAVAL TO B003-NOMAVAL
NEW-->     MOVE HR03CAD2-C-PESAVAL TO B003-PESAVAL
NEW-->     MOVE HR03CAD2-C-CGCPFAVAL TO B003-CGCPFAVAL
NEW-->     MOVE HR03CAD2-C-ESPBENS  TO B003-ESPBENS
VERI-I     MOVE HR03CAD2-C-ORIGREC1 TO B003-ORIGREC1
VERI-I     MOVE HR03CAD2-C-IDENT1   TO B003-IDENT1
NEW-->     IF  HR03CAD2-C-DATA1 NOT = ZEROS
NEW-->         PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->         MOVE HR-SQL-DATC TO B003-DATA1
NEW-->         MOVE ZEROS TO HR-NULL-C-DATA1
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-DATA1.
NEW-->     MOVE HR03CAD2-C-ORIGREC2 TO B003-ORIGREC2
NEW-->     MOVE HR03CAD2-C-IDENT2 TO B003-IDENT2
NEW-->     IF  HR03CAD2-C-DATA2 NOT = ZEROS
NEW-->         MOVE HR03CAD2-C-DATA2 TO HR-DATC-YMD
NEW-->         PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->         MOVE HR-SQL-DATC TO B003-DATA2
NEW-->         MOVE ZEROS TO HR-NULL-C-DATA2
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-DATA2.
NEW-->     MOVE HR03CAD2-C-ORIGREC3 TO B003-ORIGREC3
NEW-->     MOVE HR03CAD2-C-IDENT3 TO B003-IDENT3
NEW-->     IF  HR03CAD2-C-DATA3 NOT = ZEROS
NEW-->         MOVE HR03CAD2-C-DATA3 TO HR-DATC-YMD
NEW-->         PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->         MOVE HR-SQL-DATC TO B003-DATA3
NEW-->         MOVE ZEROS TO HR-NULL-C-DATA3
NEW-->         MOVE -1 TO HR-NULL-C-DATA3.
NEW-->     MOVE HR03CAD2-C-ORIGREC4 TO B003-ORIGREC4
NEW-->     MOVE HR03CAD2-C-IDENT4 TO B003-IDENT4
NEW-->     IF  HR03CAD2-C-DATA4 NOT = ZEROS
NEW-->         MOVE HR03CAD2-C-DATA4 TO HR-DATC-YMD
NEW-->         PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->         MOVE HR-SQL-DATC TO B003-DATA4
NEW-->         MOVE ZEROS TO HR-NULL-C-DATA4
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-DATA4.
NEW-->     MOVE HR03CAD2-C-IDENTREC TO B003-IDENTREC
NEW-->     MOVE HR03CAD2-C-PLREFINAM TO B003-PLREFINAM
NEW-->     MOVE HR03CAD2-C-SPRGREFIN TO B003-SPRGREFIN
NEW-->     IF  HR03CAD2-C-DTREFINAM NOT = ZEROS
NEW-->         MOVE HR03CAD2-C-DTREFINAM TO HR-DATC-YMD
NEW-->         PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->         MOVE HR-SQL-DATC TO B003-DTREFINAM
NEW-->         MOVE ZEROS TO HR-NULL-C-DTREFINAM
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-DTREFINAM.
NEW-->     MOVE HR03CAD2-C-TXREFIN TO B003-TXREFIN
NEW-->     MOVE HR03CAD2-C-POSTO TO B003-POSTO
NEW-->     MOVE HR03CAD2-C-APLIC TO B003-APLIC
NEW-->     MOVE HR03CAD2-C-FXPRODUT TO B003-FXPRODUT
NEW-->     MOVE HR03CAD2-C-VLBASCUST TO B003-VLBASCUST
NEW-->     MOVE HR03CAD2-C-LIMFINAN TO B003-LIMFINAN
NEW-->     MOVE HR03CAD2-C-VLCOBPRO TO B003-VLCOBPRO
NEW-->     MOVE HR03CAD2-C-TOTCOBPRO TO B003-TOTCOBPRO
NEW-->     MOVE HR03CAD2-C-PRECPRODT TO B003-PRECPRODT
NEW-->     MOVE HR03CAD2-C-PRECEMBAL TO B003-PRECEMBAL
NEW-->     MOVE HR03CAD2-C-LIMPRODUC TO B003-LIMPRODUC
NEW-->     MOVE HR03CAD2-C-LIMPRODUT TO B003-LIMPRODUT
NEW-->     IF  HR03CAD2-C-VFINAL NOT = ZEROS
NEW-->         MOVE HR03CAD2-C-VFINAL TO HR-DATC-YMD
NEW-->         PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->         MOVE HR-SQL-DATC TO B003-VFINAL
NEW-->         MOVE ZEROS TO HR-NULL-C-VFINAL
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-VFINAL.
NEW-->     MOVE HR03CAD2-C-JUROS TO B003-JUROS
NEW-->     MOVE HR03CAD2-C-CORRECAO TO B003-CORRECAO
NEW-->     MOVE HR03CAD2-C-ADPRAGRO TO B003-ADPRAGRO
NEW-->     MOVE HR03CAD2-C-INCPAGRO TO B003-INCPAGRO
NEW-->     MOVE HR03CAD2-C-VLCOBERT TO B003-VLCOBERT
NEW-->     MOVE HR03CAD2-C-CODCRS TO B003-CODCRS
NEW-->     IF  HR03CAD2-C-DTACERTO NOT = ZEROS
NEW-->         MOVE HR03CAD2-C-DTACERTO TO HR-DATC-YMD
NEW-->         PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->         MOVE HR-SQL-DATC TO B003-DTACERTO
NEW-->         MOVE ZEROS TO HR-NULL-C-DTACERTO
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-DTACERTO.
NEW-->     MOVE HR03CAD2-C-CODACERT TO B003-CODACERT
NEW-->     MOVE HR03CAD2-C-VLPRINCT-CAP999 TO B003-VLPRINCT-CAP9
NEW-->     MOVE HR03CAD2-C-VLPRINAC-PAG999 TO B003-VLPRINAC-PAG9
NEW-->     MOVE HR03CAD2-C-VLMUTORI-CAP999 TO B003-VLMUTORI-CAP9
NEW-->     MOVE HR03CAD2-C-VLMUTCOR-PAG999 TO B003-VLMUTCOR-PAG9
NEW-->     MOVE HR03CAD2-C-VLPROCTA-CAP999 TO B003-VLPROCTA-CAP9
NEW-->     MOVE HR03CAD2-C-VLPROACE-PAG999 TO B003-VLPROACE-PAG9
NEW-->     MOVE HR03CAD2-C-VLPRMUOR-CAP999 TO B003-VLPRMUOR-CAP9
NEW-->     MOVE HR03CAD2-C-VLPRMUAC-PAG999 TO B003-VLPRMUAC-PAG9
NEW-->     MOVE HR03CAD2-C-VLAUXIL-OPER999 TO B003-VLAUXIL-OPER9
NEW-->     MOVE HR03CAD2-C-JURPGORI-CAP999 TO B003-JURPGORI-CAP9
NEW-->     MOVE HR03CAD2-C-CORRPGOR-CAP999 TO B003-CORRPGOR-CAP9
NEW-->     MOVE HR03CAD2-C-CORRPGAC-PAG999 TO B003-CORRPGAC-PAG9
NEW-->     MOVE HR03CAD2-C-PROAPGOR-CAP999 TO B003-PROAPGOR-CAP9
NEW-->     MOVE HR03CAD2-C-PROAPGAC-PAG999 TO B003-PROAPGAC-PAG9
NEW-->     MOVE HR03CAD2-C-TECNPGOR-CAP999 TO B003-TECNPGOR-CAP9
NEW-->     MOVE HR03CAD2-C-TECNPGAC-PAG999 TO B003-TECNPGAC-PAG9
NEW-->     MOVE HR03CAD2-C-PERIPGOR-CAP999 TO B003-PERIPGOR-CAP9
NEW-->     MOVE HR03CAD2-C-PERIPGAC-PAG999 TO B003-PERIPGAC-PAG9
NEW-->     MOVE HR03CAD2-C-MEDIPGOR-CAP999 TO B003-MEDIPGOR-CAP9
NEW-->     MOVE HR03CAD2-C-MEDIPGAC-PAG999 TO B003-MEDIPGAC-PAG9
NEW-->     MOVE HR03CAD2-C-REMUPGOR-CAP999 TO B003-REMUPGOR-CAP9
NEW-->     MOVE HR03CAD2-C-REMUPGAC-PAG999 TO B003-REMUPGAC-PAG9
NEW-->     MOVE HR03CAD2-C-IOFPGORI-CAP999 TO B003-IOFPGORI-CAP9
NEW-->     MOVE HR03CAD2-C-IOFPGAC-PAG999 TO B003-IOFPGAC-PAG9
NEW-->     MOVE HR03CAD2-C-VLPRINCT-CAP998 TO B003-VLPRINCT-CAP8
NEW-->     MOVE HR03CAD2-C-VLPRINAC-PAG998 TO B003-VLPRINAC-PAG8
NEW-->     MOVE HR03CAD2-C-VLMUTORI-CAP998 TO B003-VLMUTORI-CAP8
NEW-->     MOVE HR03CAD2-C-VLMUTCOR-PAG998 TO B003-VLMUTCOR-PAG8
NEW-->     MOVE HR03CAD2-C-VLPROCTA-CAP998 TO B003-VLPROCTA-CAP8
NEW-->     MOVE HR03CAD2-C-VLPROACE-PAG998 TO B003-VLPROACE-PAG8
NEW-->     MOVE HR03CAD2-C-VLPRMUOR-CAP998 TO B003-VLPRMUOR-CAP8
NEW-->     MOVE HR03CAD2-C-VLAUXIL-OPER998 TO B003-VLAUXIL-OPER8
NEW-->     MOVE HR03CAD2-C-JURPGORI-CAP998 TO B003-JURPGORI-CAP8
NEW-->     MOVE HR03CAD2-C-JURPGAC-PAG998 TO B003-JURPGAC-PAG8
NEW-->     MOVE HR03CAD2-C-CORRPGOR-CAP998 TO B003-CORRPGOR-CAP8
NEW-->     MOVE HR03CAD2-C-CORRPGAC-PAG998 TO B003-CORRPGAC-PAG8
NEW-->     MOVE HR03CAD2-C-PROAPGOR-CAP998 TO B003-PROAPGOR-CAP8
NEW-->     MOVE HR03CAD2-C-PROAPGAC-PAG998 TO B003-PROAPGAC-PAG8
NEW-->     MOVE HR03CAD2-C-TECNPGOR-CAP998 TO B003-TECNPGOR-CAP8
NEW-->     MOVE HR03CAD2-C-TECNPGAC-PAG998 TO B003-TECNPGAC-PAG8
NEW-->     MOVE HR03CAD2-C-PERIPGOR-CAP998 TO B003-PERIPGOR-CAP8
NEW-->     MOVE HR03CAD2-C-PERIPGAC-PAG998 TO B003-PERIPGAC-PAG8
NEW-->     MOVE HR03CAD2-C-MEDIPGOR-CAP998 TO B003-MEDIPGOR-CAP8
NEW-->     MOVE HR03CAD2-C-MEDIPGAC-PAG998 TO B003-MEDIPGAC-PAG8
NEW-->     MOVE HR03CAD2-C-REMUPGOR-CAP998 TO B003-REMUPGOR-CAP8
NEW-->     MOVE HR03CAD2-C-REMUPGAC-PAG998 TO B003-REMUPGAC-PAG8
NEW-->     MOVE HR03CAD2-C-IOFPGORI-CAP998 TO B003-IOFPGORI-CAP8
NEW-->     MOVE HR03CAD2-C-IOFPGAC-PAG998 TO B003-IOFPGAC-PAG8
NEW-->     EXEC SQL
NEW-->       UPDATE DB2PRD.RUCA_CAD_ROOT SET
NEW-->         B003_CONTROL = :B003-CONTROL,
NEW-->         B003_NOMUTUAR1 = :B003-NOMUTUAR1,
NEW-->         B003_CGCPF1 = :B003-CGCPF1,
NEW-->         B003_NOMUTUAR2 = :B003-NOMUTUAR2,
NEW-->         B003_PESSOA2 = :B003-PESSOA2,
NEW-->         B003_CGCPF2 = :B003-CGCPF2,
NEW-->         B003_NOMUTUAR3 = :B003-NOMUTUAR3,
NEW-->         B003_PESSOA3 = :B003-PESSOA3,
NEW-->         B003_CGCPF3 = :B003-CGCPF3,
NEW-->         B003_NUMDEPEN = :B003-NUMDEPEN,
NEW-->         B003_TPDEPEN = :B003-TPDEPEN,
NEW-->         B003_MICREGIAO = :B003-MICREGIAO,
NEW-->         B003_UF = :B003-UF,
NEW-->         B003_REGIAO = :B003-REGIAO,
NEW-->         B003_EMERG = :B003-EMERG,
NEW-->         B003_ANOPROD = :B003-ANOPROD,
NEW-->         B003_VLPROD = :B003-VLPROD,
NEW-->         B003_QTDMVR = :B003-QTDMVR,
NEW-->         B003_INDENIZ = :B003-INDENIZ,
NEW-->         B003_AGENAVAL = :B003-AGENAVAL,
NEW-->         B003_CTAAVAL = :B003-CTAAVAL,
NEW-->         B003_NOMAVAL = :B003-NOMAVAL,
NEW-->         B003_PESAVAL = :B003-PESAVAL,
NEW-->         B003_ESPBENS  = :B003-ESPBENS,
VERI-I         B003_ORIGREC1 = :B003-ORIGREC1,
NEW-->         B003_IDENT1   = :B003-IDENT1,
NEW-->         B003_DATA1    = :B003-DATA1
NEW-->                         :HR-NULL-C-DATA1,
NEW-->         B003_ORIGREC2 = :B003-ORIGREC2,
NEW-->         B003_IDENT2 = :B003-IDENT2,
NEW-->         B003_DATA2 = :B003-DATA2
NEW-->                        :HR-NULL-C-DATA2,
NEW-->         B003_ORIGREC3 = :B003-ORIGREC3,
NEW-->         B003_IDENT3 = :B003-IDENT3,
NEW-->         B003_DATA3 = :B003-DATA3
NEW-->                        :HR-NULL-C-DATA3,
NEW-->         B003_ORIGREC4 = :B003-ORIGREC4,
NEW-->         B003_IDENT4 = :B003-IDENT4,
NEW-->         B003_DATA4 = :B003-DATA4
NEW-->                        :HR-NULL-C-DATA4,
NEW-->         B003_IDENTREC = :B003-IDENTREC,
NEW-->         B003_PLREFINAM = :B003-PLREFINAM,
NEW-->         B003_SPRGREFIN = :B003-SPRGREFIN,
NEW-->         B003_DTREFINAM = :B003-DTREFINAM
NEW-->         B003_TXREFIN = :B003-TXREFIN,
NEW-->         B003_NCARTREF = :B003-NCARTREF,
NEW-->         B003_POSTO = :B003-POSTO,
NEW-->         B003_APLIC = :B003-APLIC,
NEW-->         B003_FXPRODUT = :B003-FXPRODUT,
NEW-->         B003_VLBASCUST = :B003-VLBASCUST,
NEW-->         B003_LIMFINAN = :B003-LIMFINAN,
NEW-->         B003_VLCOBPRO = :B003-VLCOBPRO,
NEW-->         B003_TOTCOBPRO = :B003-TOTCOBPRO,
NEW-->         B003_PRECPRODT = :B003-PRECPRODT,
NEW-->         B003_PRECEMBAL = :B003-PRECEMBAL,
NEW-->         B003_LIMPRODUC = :B003-LIMPRODUC,
NEW-->         B003_LIMPRODUT = :B003-LIMPRODUT,
NEW-->         B003_VFINAL = :B003-VFINAL
NEW-->                        :HR-NULL-C-VFINAL,
NEW-->         B003_JUROS = :B003-JUROS,
NEW-->         B003_CORRECAO = :B003-CORRECAO,
NEW-->         B003_ADPRAGRO = :B003-ADPRAGRO,
NEW-->         B003_COBERTU = :B003-COBERTU,
NEW-->         B003_INCPAGRO = :B003-INCPAGRO,
NEW-->         B003_VLCOBERT = :B003-VLCOBERT,
NEW-->         B003_DTACERTO = :B003-DTACERTO
NEW-->                        :HR-NULL-C-DTACERTO,
NEW-->         B003_CODACERT = :B003-CODACERT,
NEW-->         B003_VLPRINCT_CAP9 = :B003-VLPRINCT-CAP9,
NEW-->         B003_VLPRINAC_PAG9 = :B003-VLPRINAC-PAG9,
NEW-->         B003_VLMUTORI_CAP9 = :B003-VLMUTORI-CAP9,
NEW-->         B003_VLMUTCOR_PAG9 = :B003-VLMUTCOR-PAG9,
NEW-->         B003_VLPROCTA_CAP9 = :B003-VLPROCTA-CAP9,
NEW-->         B003_VLPROACE_PAG9 = :B003-VLPROACE-PAG9,
NEW-->         B003_VLPRMUOR_CAP9 = :B003-VLPRMUOR-CAP9,
NEW-->         B003_VLPRMUAC_PAG9 = :B003-VLPRMUAC-PAG9,
NEW-->         B003_VLAUXIL_OPER9 = :B003-VLAUXIL-OPER9,
NEW-->         B003_JURPGORI_CAP9 = :B003-JURPGORI-CAP9,
NEW-->         B003_JURPGAC_PAG9 = :B003-JURPGAC-PAG9,
NEW-->         B003_CORRPGOR_CAP9 = :B003-CORRPGOR-CAP9,
NEW-->         B003_CORRPGAC_PAG9 = :B003-CORRPGAC-PAG9,
NEW-->         B003_PROAPGOR_CAP9 = :B003-PROAPGOR-CAP9,
NEW-->         B003_PROAPGAC_PAG9 = :B003-PROAPGAC-PAG9,
NEW-->         B003_TECNPGOR_CAP9 = :B003-TECNPGOR-CAP9,
NEW-->         B003_TECNPGAC_PAG9 = :B003-TECNPGAC-PAG9,
NEW-->         B003_PERIPGOR_CAP9 = :B003-PERIPGOR-CAP9,
NEW-->         B003_MEDIPGOR_CAP9 = :B003-MEDIPGOR-CAP9,
NEW-->         B003_MEDIPGAC_PAG9 = :B003-MEDIPGAC-PAG9,
NEW-->         B003_REMUPGOR_CAP9 = :B003-REMUPGOR-CAP9,
NEW-->         B003_REMUPGAC_PAG9 = :B003-REMUPGAC-PAG9,
NEW-->         B003_IOFPGORI_CAP9 = :B003-IOFPGORI-CAP9,
NEW-->         B003_IOFPGAC_PAG9 = :B003-IOFPGAC-PAG9,
NEW-->         B003_VLPRINCT_CAP8 = :B003-VLPRINCT-CAP8,
NEW-->         B003_VLPRINAC_PAG8 = :B003-VLPRINAC-PAG8,
NEW-->         B003_VLMUTORI_CAP8 = :B003-VLMUTORI-CAP8,
NEW-->         B003_VLMUTCOR_PAG8 = :B003-VLMUTCOR-PAG8,
NEW-->         B003_VLPROCTA_CAP8 = :B003-VLPROCTA-CAP8,
NEW-->         B003_VLPROACE_PAG8 = :B003-VLPROACE-PAG8,
NEW-->         B003_VLPRMUOR_CAP8 = :B003-VLPRMUOR-CAP8,
NEW-->         B003_VLPRMUAC_PAG8 = :B003-VLPRMUAC-PAG8,
NEW-->         B003_VLAUXIL_OPER8 = :B003-VLAUXIL-OPER8,
NEW-->         B003_JURPGORI_CAP8 = :B003-JURPGORI-CAP8,
NEW-->         B003_JURPGAC_PAG8 = :B003-JURPGAC-PAG8,
NEW-->         B003_CORRPGOR_CAP8 = :B003-CORRPGOR-CAP8,
NEW-->         B003_CORRPGAC_PAG8 = :B003-CORRPGAC-PAG8,
NEW-->         B003_PROAPGOR_CAP8 = :B003-PROAPGOR-CAP8,
NEW-->         B003_PROAPGAC_PAG8 = :B003-PROAPGAC-PAG8,
NEW-->         B003_TECNPGAC_PAG8 = :B003-TECNPGAC-PAG8,
NEW-->         B003_PERIPGOR_CAP8 = :B003-PERIPGOR-CAP8,
NEW-->         B003_PERIPGAC_PAG8 = :B003-PERIPGAC-PAG8,
NEW-->         B003_MEDIPGOR_CAP8 = :B003-MEDIPGOR-CAP8,
NEW-->         B003_MEDIPGAC_PAG8 = :B003-MEDIPGAC-PAG8,
NEW-->         B003_REMUPGOR_CAP8 = :B003-REMUPGOR-CAP8,
NEW-->         B003_REMUPGAC_PAG8 = :B003-REMUPGAC-PAG8,
NEW-->         B003_IOFPGORI_CAP8 = :B003-IOFPGORI-CAP8,
NEW-->         B003_IOFPGAC_PAG8 = :B003-IOFPGAC-PAG8
NEW-->       WHERE
NEW-->         B003_CH_CONTR = :B003-CH-CONTR
NEW-->     END-EXEC
NEW-->     MOVE 'UPDATE' TO HR-CURRENT-SQLSTMT
NEW-->     PERFORM HR-CHECK-SQLCODE
NEW-->     IF  SQLCODE = ZEROS
NEW-->         MOVE 'Y' TO HR-PERFORM-STATUS.
NEW-->*
NEW--> HR-UPDATE-RUFIV002 SECTION.
NEW-->     MOVE 'N' TO HR-PERFORM-STATUS
NEW-->     MOVE HR03CAD1-C-CTACORR TO CCTA-CORR
NEW-->     IF  HR03CAD1-C-CTAVINC NOT = ZEROS
NEW-->         MOVE ZEROS TO HR-NULL-C-CTAVINC
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-CTAVINC.
NEW-->     IF  HR03CAD1-C-PRODUTOR NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-PRODUTOR TO CTPO-PROTR-RURAL
NEW-->         MOVE ZEROS TO HR-NULL-C-PRODUTOR
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-PRODUTOR.
NEW-->     MOVE HR03CAD1-C-AGENCIA TO CJUNC-DEPDC OF MESUV001
NEW-->     MOVE HR03CAD1-C-NUMEMPR TO CCDULA
NEW-->     IF  HR03CAD1-C-MUNIC NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-MUNIC TO CMUN-RURAL
NEW-->         MOVE ZEROS TO HR-NULL-C-MUNIC
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-MUNIC.
NEW-->     IF  HR03CAD1-C-CENTRO NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-CENTRO TO CCTRO-FSCAL-RURAL
NEW-->         MOVE ZEROS TO HR-NULL-C-CENTRO
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-CENTRO.
NEW-->     MOVE HR03CAD1-C-AREATOT TO MAREA-TOT
NEW-->     IF  HR03CAD1-C-FIANCA NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-FIANCA TO CGARTD
NEW-->         MOVE ZEROS TO HR-NULL-C-FIANCA
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-FIANCA.
NEW-->     IF  HR03CAD1-C-TITULO NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-TITULO TO CESPCE-TITLO-GARNT
NEW-->         MOVE ZEROS TO HR-NULL-C-TITULO
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-TITULO.
NEW-->     IF  HR03CAD1-C-GARANTIA NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-GARANTIA TO CTPO-GARNT
NEW-->         MOVE ZEROS TO HR-NULL-C-GARANTIA
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-GARANTIA.
NEW-->     MOVE HR03CAD1-C-QTDBENS TO QTOT-GARNT
NEW-->     MOVE HR03CAD1-C-VLBENS TO VTOT-GARNT
NEW-->     MOVE HR03CAD1-C-NUMRECOR TO CREFT-BACEN
NEW-->     IF  HR03CAD1-C-NUMAUTOR NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-NUMAUTOR TO CAUTRZ-OPER
NEW-->         MOVE ZEROS TO HR-NULL-C-NUMAUTOR
NEW-->         MOVE -1 TO HR-NULL-C-NUMAUTOR.
NEW-->     IF  HR03CAD1-C-FISCALIZ NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-FISCALIZ TO CTPO-CONDC-FSCAL
NEW-->         MOVE ZEROS TO HR-NULL-C-FISCALIZ
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-FISCALIZ.
NEW-->     IF  HR03CAD1-C-ATIVID NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-ATIVID TO CATVDD-RURAL OF RUFIV002
NEW-->         MOVE ZEROS TO HR-NULL-C-ATIVID
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-ATIVID.
NEW-->     IF  HR03CAD1-C-FINALID NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-FINALID TO CFNALD-RURAL OF RUFIV002
NEW-->         MOVE ZEROS TO HR-NULL-C-FINALID
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-FINALID.
NEW-->     IF  HR03CAD1-C-EMPREEND NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-EMPREEND TO CPRODT-RURAL
NEW-->         MOVE ZEROS TO HR-NULL-C-EMPREEND
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-EMPREEND.
NEW-->     IF  HR03CAD1-C-UNIDADE1 NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-UNIDADE1 TO CUND-MEDD-FINCD
NEW-->         MOVE ZEROS TO HR-NULL-C-UNIDADE1
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-UNIDADE1.
NEW-->     MOVE HR03CAD1-C-PRODPROV TO QUND-PROD-PROVV
NEW-->     IF  HR03CAD1-C-UNIDADE2 NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-UNIDADE2 TO CUND-MEDD-PROVV
NEW-->         MOVE ZEROS TO HR-NULL-C-UNIDADE2
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-UNIDADE2.
NEW-->     IF  HR03CAD1-C-SAFRA-INI NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-SAFRA-INI TO DINIC-SAFRA
NEW-->         MOVE ZEROS TO HR-NULL-C-SAFRA-INI
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-SAFRA-INI.
NEW-->     IF  HR03CAD1-C-SAFRA-FIM NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-SAFRA-FIM TO DFIM-SAFRA
NEW-->         MOVE ZEROS TO HR-NULL-C-SAFRA-FIM
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-SAFRA-FIM.
NEW-->     MOVE HR03CAD1-C-RECPROPR TO VREC-PPRIO
NEW-->     MOVE HR03CAD1-C-CODINCID TO CINCID-PROAGRO
NEW-->     MOVE HR03CAD1-C-DTVENCTO TO HR-DATC-YMD
NEW-->     PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->     MOVE HR-SQL-DATC TO DVCTO-ORIGN-CONTR
NEW-->     MOVE HR03CAD1-C-DTCEDULA TO HR-DATC-YMD
NEW-->     PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->     MOVE HR-SQL-DATC TO DCDULA
NEW-->     MOVE HR03CAD1-C-DTOPER TO HR-DATC-YMD
NEW-->     PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->     MOVE HR-SQL-DATC TO DVCTO-CONTR
NEW-->     MOVE HR03CAD1-C-DTREGIST TO HR-DATC-YMD
NEW-->     PERFORM HR-CVDATC-YMD-TO-SQL
NEW-->     MOVE HR-SQL-DATC TO DINCL OF RUFIV002
NEW-->     IF  HR03CAD1-C-CLASPRODT NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-CLASPRODT TO CCLASF-PRODT-RURAL
NEW-->         MOVE ZEROS TO HR-NULL-C-CLASPRODT
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-CLASPRODT.
NEW-->     IF  HR03CAD1-C-CODEMBAL NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-CODEMBAL TO CTPO-EMBAL
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-CODEMBAL.
NEW-->     IF  HR03CAD1-C-CATEGMUT NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-CATEGMUT TO CCATEG-MUTUA
NEW-->         MOVE ZEROS TO HR-NULL-C-CATEGMUT
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-CATEGMUT.
NEW-->     MOVE HR03CAD1-C-CAPPRODUC TO QCAPAC-PROD
NEW-->     IF  HR03CAD1-C-EGFOUTROS NOT = ZEROS
NEW-->         MOVE HR03CAD1-C-EGFOUTROS TO VEGF
NEW-->         MOVE ZEROS TO HR-NULL-C-EGFOUTROS
NEW-->     ELSE
NEW-->         MOVE -1 TO HR-NULL-C-EGFOUTROS.
NEW-->     EXEC SQL
VERI-I       UPDATE DB2PRD.V01CONTRATO_RURAL  SET
NEW-->         CCTA_CORR = :CCTA-CORR,
NEW-->         CCTA_CTBIL_VINC = :CCTA-CTBIL-VINC
NEW-->                        :HR-NULL-C-CTAVINC,
NEW-->         CTPO_PROTR_RURAL = :CTPO-PROTR-RURAL
NEW-->                        :HR-NULL-C-PRODUTOR,
NEW-->         CJUNC_DEPDC = :MESUV001.CJUNC-DEPDC,
NEW-->         CMUN_RURAL = :CMUN-RURAL
NEW-->                        :HR-NULL-C-MUNIC,
NEW-->         CCTRO_FSCAL_RURAL = :CCTRO-FSCAL-RURAL
NEW-->                        :HR-NULL-C-CENTRO,
NEW-->         MAREA_TOT = :MAREA-TOT,
NEW-->         IPPRIE = :IPPRIE,
NEW-->         CGARTD = :CGARTD
NEW-->                        :HR-NULL-C-FIANCA,
NEW-->         CESPCE_TITLO_GARNT = :CESPCE-TITLO-GARNT
NEW-->                        :HR-NULL-C-TITULO,
NEW-->         CTPO_GARNT = :CTPO-GARNT
NEW-->                        :HR-NULL-C-GARANTIA,
NEW-->         QTOT_GARNT = :QTOT-GARNT,
NEW-->         VTOT_GARNT = :VTOT-GARNT,
NEW-->         CORIGE_REC = :RUFIV002.CORIGE-REC
NEW-->                        :HR-NULL-C-ORIGREC1,
NEW-->         CCONDC_ESPCL_RURAL = :CCONDC-ESPCL-RURAL,
NEW-->         CREFT_BACEN = :CREFT-BACEN,
NEW-->         CAUTRZ_OPER = :CAUTRZ-OPER
NEW-->                        :HR-NULL-C-NUMAUTOR,
NEW-->         CTPO_CONDC_FSCAL = :CTPO-CONDC-FSCAL
NEW-->         CATVDD_RURAL = :RUFIV002.CATVDD-RURAL
NEW-->                        :HR-NULL-C-ATIVID,
NEW-->         CFNALD_RURAL = :RUFIV002.CFNALD-RURAL
NEW-->                        :HR-NULL-C-FINALID,
NEW-->         CPRODT_RURAL = :CPRODT-RURAL
NEW-->                        :HR-NULL-C-EMPREEND,
NEW-->         QUND_FINCD_RURAL = :QUND-FINCD-RURAL,
NEW-->         CUND_MEDD_FINCD = :CUND-MEDD-FINCD
NEW-->                        :HR-NULL-C-UNIDADE1,
NEW-->         QUND_PROD_PROVV = :QUND-PROD-PROVV,
NEW-->         CUND_MEDD_PROVV = :CUND-MEDD-PROVV
NEW-->                        :HR-NULL-C-UNIDADE2,
NEW-->         DINIC_SAFRA = :DINIC-SAFRA
NEW-->                        :HR-NULL-C-SAFRA-INI,
NEW-->         DFIM_SAFRA = :DFIM-SAFRA
NEW-->                        :HR-NULL-C-SAFRA-FIM,
NEW-->         VFINAN = :VFINAN,
NEW-->         VREC_PPRIO = :VREC-PPRIO,
NEW-->         CINCID_PROAGRO = :CINCID-PROAGRO,
NEW-->         DVCTO_ORIGN_CONTR = :DVCTO-ORIGN-CONTR,
NEW-->         DCDULA = :DCDULA,
NEW-->         DINCL = :RUFIV002.DINCL,
NEW-->         CCLASF_PRODT_RURAL = :CCLASF-PRODT-RURAL
NEW-->                        :HR-NULL-C-CLASPRODT,
NEW-->         CTPO_EMBAL = :CTPO-EMBAL
NEW-->                        :HR-NULL-C-CODEMBAL,
NEW-->         CCATEG_MUTUA = :CCATEG-MUTUA
NEW-->                        :HR-NULL-C-CATEGMUT,
NEW-->         QCAPAC_PROD = :QCAPAC-PROD,
NEW-->         VEGF = :VEGF
NEW-->                        :HR-NULL-C-EGFOUTROS
NEW-->       WHERE
NEW-->         CCONTR = :RUFIV002.CCONTR
NEW-->     END-EXEC
NEW-->     MOVE 'UPDATE' TO HR-CURRENT-SQLSTMT
NEW-->     PERFORM HR-CHECK-SQLCODE
NEW-->     IF  SQLCODE = ZEROS
NEW-->         MOVE 'Y' TO HR-PERFORM-STATUS.
NEW-->*
NEW--> HR-COND-SELECT-RUCAB003 SECTION.
NEW-->     IF  NOT HR-PREVIOUS-OK
NEW-->         GO TO HR-COND-SELECT-RUCAB003-END.
NEW-->     PERFORM HR-SELECT-RUCAB003.
NEW--> HR-COND-SELECT-RUCAB003-END. EXIT.
NEW-->*
NEW--> HR-COND-UPDATE-RUCAB003 SECTION.
NEW-->     IF  NOT HR-PREVIOUS-OK
NEW-->         GO TO HR-COND-UPDATE-RUCAB003-END.
NEW-->     MOVE HR03CAD1-C-CH-CONTR TO B003-CH-CONTR
NEW-->     PERFORM HR-UPDATE-RUCAB003.
NEW-->     MOVE SQLCODE TO HR-PRIMARY-SQLCODE.
NEW--> HR-COND-UPDATE-RUCAB003-END. EXIT.
NEW-->*
NEW--> HR-CLOSE-RUCAB004-003-04 SECTION.
NEW-->     IF  HR-CURSOR-RUCAB004-003-04 = 'OPENED'
NEW-->         EXEC SQL CLOSE HR_RUCAB004_003_04 END-EXEC
NEW-->         MOVE 'CLOSE ' TO HR-CURRENT-SQLSTMT
NEW-->         PERFORM HR-CHECK-SQLCODE
NEW-->         MOVE 'CLOSED' TO HR-CURSOR-RUCAB004-003-04.
NEW-->*
NEW--> HR-SET-PCBSTAT SECTION.
NEW-->     IF  HR-PRIMARY-SQLCODE = ZEROS
NEW-->         MOVE '  ' TO HR-PCB-STATCD
NEW-->     IF  HR-PRIMARY-SQLCODE = +100
NEW-->         IF  HR-CALL-ID = 'N'
NEW-->             MOVE 'GB' TO HR-PCB-STATCD
NEW-->         ELSE
NEW-->             MOVE 'GE' TO HR-PCB-STATCD
NEW-->     ELSE
NEW-->     IF  HR-PRIMARY-SQLCODE = -803
NEW-->         MOVE 'II' TO HR-PCB-STATCD
NEW-->     ELSE
NEW-->         MOVE 'ZZ' TO HR-PCB-STATCD .
NEW-->*
NEW--> HR-CHECK-SQLCODE SECTION.
NEW-->     IF  NOT (SQLCODE = ZEROS OR +100 OR -803)
NEW-->         MOVE SQLCODE TO HR-EDITED-SQLCODE
NEW-->         DISPLAY HR-PROGRAM-ID
NEW-->                 ' * INVALID SQLCODE '
NEW-->                 HR-EDITED-SQLCODE
NEW-->                 ' AT STATEMENT '
NEW-->                 HR-CURRENT-SQLSTMT
NEW-->                 ' - CALL '
NEW-->                 HR-CALL-NUM
NEW-->                 HR-CALL-ID
NEW-->                 UPON CONSOLE
NEW-->         DISPLAY HR-PROGRAM-ID
NEW-->                 ' * SQLERRM ==> '
NEW-->                 SQLERRMC
NEW-->                 UPON CONSOLE .
NEW-->*
NEW--> HR-CVDATC-SQL-TO-YMD SECTION.
NEW-->     IF  HR-SQL-DATC-EUR-P1 = '.'
NEW-->         MOVE HR-SQL-DATC-EUR-YY TO HR-DATC-YMD-YY
NEW-->         MOVE HR-SQL-DATC-EUR-MM TO HR-DATC-YMD-MM
NEW-->         MOVE HR-SQL-DATC-EUR-DD TO HR-DATC-YMD-DD
NEW-->     ELSE
NEW-->     IF  HR-SQL-DATC-EUR-P1 = '/'
NEW-->         MOVE HR-SQL-DATC-USA-YY TO HR-DATC-YMD-YY
NEW-->         MOVE HR-SQL-DATC-USA-MM TO HR-DATC-YMD-MM
NEW-->         MOVE HR-SQL-DATC-USA-DD TO HR-DATC-YMD-DD
NEW-->     ELSE
NEW-->         MOVE HR-SQL-DATC-ISO-YY TO HR-DATC-YMD-YY
NEW-->         MOVE HR-SQL-DATC-ISO-MM TO HR-DATC-YMD-MM
NEW-->         MOVE HR-SQL-DATC-ISO-DD TO HR-DATC-YMD-DD .
NEW--> HR-CVDATC-YMD-TO-SQL SECTION.
NEW-->     MOVE 'DD.MM.YYYY' TO HR-SQL-DATC
NEW-->     MOVE HR-DATC-YMD-YY TO HR-SQL-DATC-EUR-YY
NEW-->     MOVE HR-DATC-YMD-MM TO HR-SQL-DATC-EUR-MM
NEW-->     MOVE HR-DATC-YMD-DD TO HR-SQL-DATC-EUR-DD .
