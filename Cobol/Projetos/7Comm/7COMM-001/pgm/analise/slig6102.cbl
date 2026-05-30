      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. SLIG6102.
       AUTHOR.     CLEOMAR DA SILVA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   SLIG6102                                    *
      *    PROGRAMADOR.:   CLEOMAR DA SILVA        - CPM PATO BRANCO   *
      *    ANALISTA CPM:   FRANCISMARA PAGNONCELLI - CPM PATO BRANCO   *
      *    ANALISTA....:   REGINALDO PEDROSO       - DTS / GP. 75      *
      *    DATA........:   31/03/2004                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   UNIFICAR REGISTROS E SUMARIZAR TITULOS POR  *
      *      CLIENTE, SACADO, CARTEIRA, DATA PERIODO INICIO E DATA PE- *
      *      RIODO FIM.                                                *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    CADACLIE                  I#DEVE17          *
      *                    ARQERROS                  I#SLIG22          *
      *                    PERIDECE                  I#SLIG00          *
      *                    CLIENDUP                  I#SLIG97          *
      *                    CLIENUNI                  I#SLIG02          *
      *                    SRELCTRL                     -              *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    ILBOABN0 - MODULO PARA ABENDAR O PROGRAMA                   *
      *    BRAD0300 - LE ARQUIVO COMPRIMIDO                            *
      *    BRAD0310 - GRAVA ARQUIVO COMPRIMIDO                         *
      *    POOL1640 - SOMA DIAS/MESES/ANOS. DATAS C/SEC                *
      *    BRAD7100 - MODULO DE TRATAMENTO DE ERRO                     *
      *    BRAD7600 - FORN. DATA E HORA CORRENTE                       *
      *================================================================*
      *                   A L T E R A C A O                            *
      *----------------------------------------------------------------*
      *  ALTERADO POR : CAMILA - DTS CONSULTING                        *
      *  DATA........ : 13/03/2005                                     *
      *  ALTERACAO....: QDO CLIENTE NAO FOR ENCONTRADO NO CADACLIE,    *
      *                 VERIFICAR SE EXISTE NO CADACBCL.               *
      *----------------------------------------------------------------*
      *  ALTERADO POR : DANIEL AMARAL PEREIRA - DTS CONSULTING         *
      *  DATA........ : 21/10/2005                                     *
      *  ALTERACAO....: VERIFICAR PRIMEIRO NO CADASTRO DO CBCL E, SE   *
      *                 NAO ENCONTRAR, VERIFICAR NO CLIE.              *
      *----------------------------------------------------------------*
FS0617*  ALTERADO POR : HENRIQUE MANDROTE - FOURSYS                    *
FS0617*  DATA........ : 29/06/2017                                     *
FS0617*  ALTERACAO....: ACERTAR MOVIMENTACAO DE VARIAVEL               *
      *----------------------------------------------------------------*
4SYS01*  ALTERADO POR : LUCAS DEL ROY   - FOURSYS                      *
4SYS01*  DATA........ : 08/05/2019                                     *
4SYS01*  ALTERACAO....: AJUSTAR A DATA DE CARENCIA DE 10 DIAS CORRIDOS *
4SYS01*                 PARA 3 DIAS UTEIS CONFORME PEDIDO DO GESTOR    *
      *----------------------------------------------------------------*
IBM001*  ALTERADO POR : JOSE AUGUSTO REGO - IBM BRASIL                 *
IBM001*  DATA........ : 05/03/2024                                     *
IBM001*  ALTERACAO....: PROJETO ATIVACAO NOVO TI. INCLUIR AJUSTE NO    *
IBM001*                 TRATAMENTO DO FIM DE ARQUIVO CADACBCL, MOVER   *
IBM001*                 9999999999999 PARA A CHAVE DO BALANCE-LINE.    *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT PERIDECE ASSIGN      TO UT-S-PERIDECE
           FILE STATUS                 IS WRK-FS-PERIDECE.

           SELECT CADACBCL ASSIGN      TO UT-S-CADACBCL
           FILE STATUS                 IS WRK-FS-CADACBCL.

           SELECT ARQERROS ASSIGN      TO UT-S-ARQERROS
           FILE STATUS                 IS WRK-FS-ARQERROS.

           SELECT SRELCTRL ASSIGN      TO UT-S-SRELCTRL
           FILE STATUS                 IS WRK-FS-SRELCTRL.

           SELECT CLIENUNI ASSIGN      TO UT-S-CLIENUNI
           FILE STATUS                 IS WRK-FS-CLIENUNI.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE DATAS                                    *
      *            ORG. SEQUENCIAL     -   LRECL = 100                 *
      *----------------------------------------------------------------*

       FD  PERIDECE
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG00

           EJECT
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE CLIENTES COBRANCA                        *
      *            ORG. SEQUENCIAL     -   LRECL = 259                 *
      *----------------------------------------------------------------*

       FD  CADACBCL
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG20

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE ERROS                                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  ARQERROS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG22

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: CONTROLE DE PROCESSAMENTO                           *
      *            ORG. SEQUENCIAL     -   LRECL   =   080             *
      *----------------------------------------------------------------*

       FD  SRELCTRL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SRELCTRL                PIC  X(080).

           EJECT
      *----------------------------------------------------------------*
      *    OUTPUT: REDUZIDO DE CLIENTES UNIFICADO                      *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  CLIENUNI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

-INC I#SLIG02

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6102 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-CADACLIE          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CADACBCL          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CLIENDUP          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ARQERROS          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-CLIENUNI          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-DESPR-CLIENDUP          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-ACHOU-CADACLIE          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-ACHOU-CADACBCL          PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-NAO-ACHOU-CADACLIE      PIC  9(013) COMP-3  VALUE ZEROS.
       77  ACU-NAO-ACHOU-CADACBCL      PIC  9(013) COMP-3  VALUE ZEROS.
CNPJAN 77  ACU-CGC-NAO-POP-SLIG97      PIC  9(013) COMP-3  VALUE ZEROS.
CNPJAN 77  ACU-CGC-NAO-POP-DEVE17      PIC  9(013) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTES DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-PERIDECE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADACBCL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ARQERROS             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CADACLIE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SRELCTRL             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CLIENUNI             PIC  X(002)         VALUE SPACES.

       77  WRK-OPERACAO                PIC  X(013)         VALUE SPACES.
       77  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA  ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-CADACLIE                PIC  X(008)         VALUE
           'CADACLIE'.
       77  WRK-CADACBCL                PIC  X(008)         VALUE
           'CADACBCL'.
       77  WRK-ARQERROS                PIC  X(008)         VALUE
           'ARQERROS'.
       77  WRK-CLIENDUP                PIC  X(008)         VALUE
           'CLIENDUP'.
       77  WRK-CLIENUNI                PIC  X(008)         VALUE
           'CLIENUNI'.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-ABEND                   PIC S9(004) COMP    VALUE +0001.
       77  WRK-RETURN-CODE             PIC  9(002)         VALUE ZEROS.
       77  WRK-FIM-CLIENDUP            PIC  X(001)         VALUE SPACES.
       77  WRK-DT-VENCTO-PROD-AUX      PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-INI-PER-R            PIC  9(008)         VALUE ZEROS.
       77  WRK-DT-FIM-PER-R            PIC  9(008)         VALUE ZEROS.
       77  WRK-NOME-ARQ                PIC  X(008)         VALUE SPACES.
       77  WRK-VL-PRINC-PROD-CONV      PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VANTES-PER-NAO-PG       PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VVCTO-PER-PG            PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VVCTO-PER-NAO-PG        PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VVCTO-PER-BXADO         PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VVCTO-APOS-PER-PG       PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VVCTO-APOS-PER-NAO-PG   PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VVCTO-APOS-PER-BXADO    PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VTOT-PRROG              PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-VTOT-ABTMT              PIC  9(015)V99 COMP-3
                                                           VALUE ZEROS.
       77  WRK-OSCILACAO               PIC S9(015)V99 COMP-3
                                                           VALUE ZEROS.

       77  WRK-NRO-GRUPO-CLIENTE       PIC  9(009)         VALUE ZEROS.
       77  WRK-TOT-GRUP-PESQ           PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-TOT-GRUP-ACHOU          PIC  9(015) COMP-3  VALUE ZEROS.
       77  WRK-TOT-GRUP-NAO-ACHOU      PIC  9(015) COMP-3  VALUE ZEROS.

       77  WRK-SLIG97-BCO-CLIENTE      PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-SLIG97-AGENCIA-CLIENTE  PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-SLIG97-CONTA-CLIENTE    PIC  9(013) COMP-3  VALUE ZEROS.
       77  WRK-SLIG97-BCO-SACADO       PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-SLIG97-AGENCIA-SACADO   PIC  9(005) COMP-3  VALUE ZEROS.
       77  WRK-SLIG97-CONTA-SACADO     PIC  9(013) COMP-3  VALUE ZEROS.

       77  WRK-NOME-CLIENTE            PIC  X(040)         VALUE SPACES.
       77  WRK-FLAG-CADACLIE           PIC  X(001)         VALUE SPACES.
       77  WRK-FLAG-CADACBCL           PIC  X(001)         VALUE SPACES.

       77  WRK-MENSA01                 PIC  X(075)         VALUE
           '** ARQUIVO CLIENDUP VAZIO **'.
       77  WRK-MENSA02                 PIC  X(075)         VALUE
           '** ARQUIVO PERIDECE VAZIO **'.
       77  WRK-MENSA03                 PIC  X(075)         VALUE
           '** SLIG00-CD-TP-PROCESSA IGUAL A 2, NAO ENCONTRADO **'.

ISF    01  WRK-DT-INI-PER-R-9          PIC  9(009) COMP-3  VALUE ZEROS.
ISF    01  WRK-DT-FIM-PER-R-9          PIC  9(009) COMP-3  VALUE ZEROS.

CNPJAN 01 WRK-GRUPO-X.
CNPJAN    05 WRK-GRUPO-N               PIC  9(09)   VALUE ZEROS.

       01  WRK-MENSA04.
           03  FILLER                  PIC  X(043)         VALUE
               '** ERRO NO ACESSO A POOL1640 RETURN-CODE = '.
           03  WRK-RET-COD-1640        PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(030)         VALUE ' **'.

       01  WRK-CGC-CPF-ANT.
           03  WRK-CGC-CPF-CLI-ANT     PIC  X(009)        VALUE SPACES.
CNPJAN     03  WRK-FILIAL-CLI-ANT      PIC  X(005)        VALUE SPACES.
           03  WRK-CONTROLE-CLI-ANT    PIC  9(002)        VALUE ZEROS.

       01  WRK-MENSA-ARQERROS.
           03  WRK-CPFCGC-ARQERROS     PIC  X(009)         VALUE ZEROS.
           03  WRK-BARRA-ARQERROS      PIC  X(001)         VALUE '/'.
           03  WRK-FILIAL-ARQERROS     PIC  X(004)         VALUE ZEROS.
           03  WRK-HIFEM-ARQERROS      PIC  X(001)         VALUE '-'.
           03  WRK-CTRL-ARQERROS       PIC  9(002)         VALUE ZEROS.
           03  WRK-MSG-ARQERROS        PIC  X(013)         VALUE
               ' SEM CADACLIE'.

       01  WRK-DT-VCT-PRD-CAR.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  WRK-DT-VCT-PRD-CAR-N    PIC  9(008)         VALUE ZEROS.

       01  WRK-DT-VCT-PRD-CAR-R        REDEFINES
           WRK-DT-VCT-PRD-CAR          PIC  X(010).

       01  WRK-VL-PG-PROD              PIC  9(015)         COMP-3.
       01  WRK-VL-PG-PROD-R            REDEFINES
           WRK-VL-PG-PROD              PIC  9(013)V99      COMP-3.

       01  WRK-REG-INTERMED-SACADO.
           03  WRK-BCO-CLIENTE         PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-AGENCIA-CLIENTE     PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTA-CLIENTE       PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-CTPO-CART           PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-CCLI-COBR           PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-CGC-CPF-CLIENTE     PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-FILIAL-CLIENTE      PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTROLE-CLIENTE    PIC  9(002)         VALUE ZEROS.
           03  WRK-BCO-SACADO          PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-AGENCIA-SACADO      PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTA-SACADO        PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-CGC-CPF-SACADO      PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-FILIAL-SACADO       PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CONTROLE-SACADO     PIC  9(002)         VALUE ZEROS.
           03  WRK-NOME-SACADO         PIC  X(040)         VALUE SPACES.
           03  WRK-CD-ORIG-NM-SAC      PIC  9(001)         VALUE ZEROS.
           03  WRK-VL-PRINC-PROD       PIC  9(017) COMP-3  VALUE ZEROS.
           03  WRK-VL-PAGO-PROD        PIC  9(015) COMP-3  VALUE ZEROS.
           03  WRK-VL-DESC-ABAT-PROD   PIC  9(015) COMP-3  VALUE ZEROS.
           03  WRK-DT-VENCTO-PROD      PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-DT-PAGTO-PROD       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-DT-ENTRADA-ORIGEM   PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-DT-ENTRADA-SISTEMA  PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-DT-VENCTO-ANTERIOR  PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-CD-PRODUTO          PIC  9(002)         VALUE ZEROS.
           03  WRK-CD-CNEGOC           PIC  9(011) COMP-3  VALUE ZEROS.
           03  WRK-CD-NOSNRO           PIC  9(011) COMP-3  VALUE ZEROS.
           03  WRK-CD-SEQUENCIA        PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-CD-COD-NTAX         PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-CD-OCO-REM-L        PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-CD-MOT-REM-SIS      PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-CD-ESPECIE          PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-CD-ORIGEM           PIC  9(003) COMP-3  VALUE ZEROS.
           03  WRK-DT-INICIO-PER       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-DT-FINAL-PER        PIC  9(009) COMP-3  VALUE ZEROS.

      * === CAMPOS DUPLICADOS PARA STRING ===
CNPJAN     03  WRK-CGC-CPF-CLIENTE-ST  PIC X(009)          VALUE SPACES.
CNPJAN     03  WRK-FILIAL-CLIENTE-ST   PIC X(005)          VALUE SPACES.
CNPJAN     03  WRK-CONTROLE-CLIENTE-ST PIC 9(002)          VALUE ZEROS.
CNPJAN     03  WRK-CGC-CPF-SACADO-ST   PIC X(009)          VALUE SPACES.
CNPJAN     03  WRK-FILIAL-SACADO-ST    PIC X(005)          VALUE SPACES.
CNPJAN     03  WRK-CONTROLE-SACADO-ST  PIC 9(002)          VALUE ZEROS.
CNPJAN     03  WRK-FILLER              PIC X(021)          VALUE SPACES.

CNPJAN*01  WRK-VALOR-HEXA-09-S         PIC +9(009) VALUE ZEROS.
CNPJAN*01  FILLER REDEFINES WRK-VALOR-HEXA-09-S.
CNPJAN*    05  FILLER                  PIC  X(001).
CNPJAN*    05  WRK-VALOR-HEXA-09-X.
CNPJAN*        07  WRK-VALOR-HEXA-09-N PIC  9(009).
CNPJAN*01  FILLER REDEFINES WRK-VALOR-HEXA-09-S.
CNPJAN*    05  FILLER                  PIC  X(005).
CNPJAN*    05  WRK-VALOR-HEXA-05-X.
CNPJAN*        07  WRK-VALOR-HEXA-05-N PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CHAVES DE ARQUIVO ***'.
      *----------------------------------------------------------------*

       01  WRK-CHV-SUMARIZA.
           03 WRK-CH-CGC-CPF-CLI       PIC  X(009)        VALUE SPACES.
           03 WRK-CH-FILIAL-CLI        PIC  X(005)        VALUE SPACES.
           03 WRK-CH-CONTROLE-CLI      PIC  9(002)        VALUE ZEROS.
           03 WRK-CH-CGC-CPF-SAC       PIC  X(009)        VALUE SPACES.
           03 WRK-CH-FILIAL-SAC        PIC  X(005)        VALUE SPACES.
           03 WRK-CH-CONTROLE-SAC      PIC  9(002)        VALUE ZEROS.
           03 WRK-CH-CTPO-CART         PIC  9(003)        VALUE ZEROS.

       01  WRK-CHV-ANTERIOR.
           03 WRK-CH-CLIENTE-ANT.
CNPJAN        05 WRK-CH-CGC-CPF-CLI-ANT   PIC  X(009)     VALUE SPACES.
CNPJAN        05 WRK-CH-FILIAL-CLI-ANT    PIC  X(005)     VALUE SPACES.
              05 WRK-CH-CONTROLE-CLI-ANT  PIC  9(002)     VALUE ZEROS.
           03 WRK-CH-SACADO-ANT.
              05 WRK-CH-CGC-CPF-SAC-ANT   PIC  X(009)     VALUE SPACES.
              05 WRK-CH-FILIAL-SAC-ANT    PIC  X(005)     VALUE SPACES.
              05 WRK-CH-CONTROLE-SAC-ANT  PIC  9(002)     VALUE ZEROS.
           03 WRK-CH-CTPO-CART-ANT        PIC  9(003)     VALUE ZEROS.

       01  WRK-CHV-DEVE.
           03 WRK-CPFCGC-DEVE      PIC  X(009)     VALUE ZEROS.
           03 WRK-FILIAL-DEVE      PIC  X(005)     VALUE ZEROS.
           03 WRK-CONTROLE-DEVE    PIC  9(002)     VALUE ZEROS.

       01  WRK-CHV-CBCL.
           03 WRK-CPFCGC-CBCL      PIC  X(009)     VALUE ZEROS.
           03 WRK-FILIAL-CBCL      PIC  X(005)     VALUE ZEROS.
           03 WRK-CONTROLE-CBCL    PIC  9(002)     VALUE ZEROS.

       01  WRK-CHV-ULTNOME.
           03 WRK-CPFCGC-ULTNOME   PIC  X(009)     VALUE ZEROS.
           03 WRK-FILIAL-ULTNOME   PIC  X(005)     VALUE ZEROS.
           03 WRK-CTRL-ULTNOME     PIC  9(002)     VALUE ZEROS.

CNPJAN 01 WRK-CNPJ-9-S                 PIC +9(09)   VALUE ZEROS.
.      01 FILLER                       REDEFINES WRK-CNPJ-9-S.
.         05 WRK-SINAL-9-X             PIC  X(01).
CNPJAN    05 WRK-CNPJ-9-X              PIC  X(09).

CNPJAN 01 WRK-FILIAL-5-S               PIC +9(05)   VALUE ZEROS.
.      01 FILLER                       REDEFINES WRK-FILIAL-5-S.
.         05 WRK-SINAL-5-X             PIC  X(01).
CNPJAN    05 WRK-FILIAL-5-X            PIC  X(05).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO A POOL1640 ***'.
      *----------------------------------------------------------------*

       01  WRK-1640-AREA.
           03 WRK-1640-DATA-ENTRADA.
              05 FILLER                PIC  X(002)         VALUE SPACES.
              05 WRK-1640-DATA-ENTRADA-N
                                       PIC  9(008)         VALUE ZEROS.
           03 WRK-1640-FORMATO-ENTRADA PIC  9(002)         VALUE ZEROS.
           03 WRK-1640-TIPO-INCREMENTO PIC  X(002)         VALUE SPACES.
           03 WRK-1640-INCREMENTO      PIC  9(003)         VALUE ZEROS.
           03 WRK-1640-FORMATO-SAIDA   PIC  9(002)         VALUE ZEROS.
           03 WRK-1640-DATA-SAIDA      PIC  X(010)         VALUE SPACES.
           03 WRK-1640-MENSAGEM        PIC  X(080)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA-7600.
           03  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           03  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO-SIS         PIC  9(004)         VALUE ZEROS.
               05  WRK-MES-SIS         PIC  9(002)         VALUE ZEROS.
               05  WRK-DIA-SIS         PIC  9(002)         VALUE ZEROS.
               05  FILLER              PIC  X(012)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO RELATORIO SRELCTRL ***'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  FILLER                  PIC  X(001)         VALUE '1'.
           03  FILLER                  PIC  X(016)         VALUE
               'BRADESCO'.
           03  FILLER                  PIC  X(053)         VALUE
               'SLIG - SISTEMA DE INDICE DE LIQUIDEZ'.
           03  FILLER                  PIC  X(010)         VALUE
               'FOL. UNICA'.

       01  CABEC2.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(069)         VALUE
               'SLIG6102'.
           03  CB2-DIA                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-MES                 PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  CB2-ANO                 PIC  9(004)         VALUE ZEROS.

       01  CABEC3.
           03  FILLER                  PIC  X(001)         VALUE SPACES.
           03  FILLER                  PIC  X(015)         VALUE SPACES.
           03  FILLER                  PIC  X(064)         VALUE
               'UNIFICAR REGISTROS E SUMARIZAR TITULOS'.

       01  CABEC4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(025)         VALUE SPACES.
           03  FILLER                  PIC  X(054)         VALUE
               'CONTROLE OPERACIONAL'.

       01  LINTOT1.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS LIDOS       (CLIENDUP) : '.
           03  LT1-LIDOS-CLIENDUP      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS LIDOS       (CADACLIE) : '.
           03  LT2-LIDOS-CADACLIE      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2A.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS LIDOS       (CADACBCL) : '.
           03  LT2-LIDOS-CADACBCL      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS GRAVADOS    (CLIENUNI) : '.
           03  LT3-GRAVA-CLIENUNI      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS GRAVADOS    (ARQERROS) : '.
           03  LT4-GRAVA-ARQERROS      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT5.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS DESPREZADOS (CLIENDUP) : '.
           03  LT5-DESPR-CLIENDUP      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT6.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGISTROS PESQUISADOS (GRUP0930) : '.
           03  LT6-GRUP-PESQ           PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT7.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS CLI ENCONTRADOS  (GRUP0930) : '.
           03  LT7-GRUP-ACHOU          PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT8.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS CLI NAO ENCONTRAD(GRUP0930) : '.
           03  LT8-GRUP-NAO-ACHOU      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT9.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS ENCONTRADOS      (CADACLIE) : '.
           03  LT9-ACHOU-CADACLIE      PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT10.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS NAO ENCONTRADOS  (CADACLIE) : '.
           03  LT10-NAO-ACHOU-CADACLIE  PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT11.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS ENCONTRADOS      (CADACBCL) : '.
           03  LT11-ACHOU-CADACBCL     PIC  ZZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT12.
           03  FILLER                  PIC  X(001)         VALUE '0'.
           03  FILLER                  PIC  X(010)         VALUE SPACES.
           03  FILLER                  PIC  X(035)         VALUE
               'REGS NAO ENCONTRADOS  (CADACBCL) : '.
           03  LT12-NAO-ACHOU-CADACBCL  PIC  ZZZ.ZZZ.ZZZ.ZZ9.

           EJECT

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO MODULO GRUP0930 ***'.
      *----------------------------------------------------------------*

       01  COMU-AMBIENTE               PIC  X(001)         VALUE SPACES.
       01  COMU-IO-PCB                 PIC  X(032)         VALUE SPACES.
       01  COMU-ALT-PCB                PIC  X(032)         VALUE SPACES.
       01  COMU-ERRO-AREA              PIC  X(107)         VALUE SPACES.

       01  COMU-PARM.
           03  COMU-ENTRADA.
               05  COMU-NOME-PGM       PIC  X(008)         VALUE SPACES.
               05  COMU-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
               05  COMU-TPO-RELAC      PIC  9(003)         VALUE ZEROS.
               05  COMU-EMPRESA        PIC  9(005)         VALUE ZEROS.
               05  COMU-DEPDC          PIC  9(005)         VALUE ZEROS.
           03  COMU-SAIDA.
               05  COMU-EMPR-RELAC     PIC  9(005)         VALUE ZEROS.
               05  COMU-DEPTO-RELAC    PIC  9(005)         VALUE ZEROS.
               05  COMU-NOME-DEPDC     PIC  X(050)         VALUE SPACES.

       COPY GRUPW01S.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** INCIO DA INC I#DEVE17 DO CADACLIE   ***'.
      *----------------------------------------------------------------*

      *-INC I#DEVE17
       COPY I#DEVEN7.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** LAYOUT DO ARQUIVO CLIENDUP ***'.
      *----------------------------------------------------------------*

-INC I#SLIG97

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

-INC POL7100C

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** SLIG6102 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FIM-CLIENDUP    EQUAL 'S'
                OR WRK-FS-CADACBCL     EQUAL '10'
                OR WRK-FS-CADACLIE     EQUAL 'S'
           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  PERIDECE CADACBCL
                OUTPUT CLIENUNI ARQERROS SRELCTRL.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           PERFORM 120000-ACESSAR-BRAD7600.

           MOVE  LOW-VALUES            TO WRK-CGC-CPF-CLI-ANT
                                          WRK-FILIAL-CLI-ANT
                                          WRK-CHV-SUMARIZA
                                          WRK-CHV-ANTERIOR
                                          WRK-CHV-DEVE
                                          WRK-CHV-CBCL.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-PERIDECE.

           PERFORM 112000-TESTAR-FS-SRELCTRL.

           PERFORM 113000-TESTAR-FS-CLIENUNI.

           PERFORM 114000-TESTAR-FS-ARQERROS.

           PERFORM 115000-TESTAR-FS-CADACBCL.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-PERIDECE       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-PERIDECE         NOT EQUAL '00'
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*        ERRO ' WRK-OPERACAO
                                                  ' DO ARQUIVO       *'
               DISPLAY '*                  PERIDECE                  *'
               DISPLAY '*              FILE STATUS = ' WRK-FS-PERIDECE
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               CALL 'ILBOABN0'         USING WRK-ABEND
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-SRELCTRL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SRELCTRL         NOT EQUAL ZEROS
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*        ERRO ' WRK-OPERACAO
                                                  ' DO ARQUIVO       *'
               DISPLAY '*                  SRELCTRL                  *'
               DISPLAY '*              FILE STATUS = ' WRK-FS-SRELCTRL
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               CALL 'ILBOABN0'         USING WRK-ABEND
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       113000-TESTAR-FS-CLIENUNI       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CLIENUNI         NOT EQUAL ZEROS
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*        ERRO ' WRK-OPERACAO
                                                  ' DO ARQUIVO       *'
               DISPLAY '*                  CLIENUNI                  *'
               DISPLAY '*              FILE STATUS = ' WRK-FS-CLIENUNI
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               CALL 'ILBOABN0'         USING WRK-ABEND
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       114000-TESTAR-FS-ARQERROS       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQERROS         NOT EQUAL ZEROS
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*        ERRO ' WRK-OPERACAO
                                                  ' DO ARQUIVO       *'
               DISPLAY '*                  ARQERROS                  *'
               DISPLAY '*              FILE STATUS = ' WRK-FS-ARQERROS
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               CALL 'ILBOABN0'         USING WRK-ABEND
           END-IF.

      *----------------------------------------------------------------*
       114000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       115000-TESTAR-FS-CADACBCL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CADACBCL         NOT EQUAL '00'
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*        ERRO ' WRK-OPERACAO
                                                  ' DO ARQUIVO       *'
               DISPLAY '*                  CADACBCL                  *'
               DISPLAY '*              FILE STATUS = ' WRK-FS-CADACBCL
                                                      '              *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               CALL 'ILBOABN0'         USING WRK-ABEND
           END-IF.

      *----------------------------------------------------------------*
       115000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

           EJECT
      *----------------------------------------------------------------*
       120000-ACESSAR-BRAD7600         SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA-7600.

           MOVE WRK-ANO-SIS            TO CB2-ANO.
           MOVE WRK-MES-SIS            TO CB2-MES.
           MOVE WRK-DIA-SIS            TO CB2-DIA.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-CLIENDUP.

           IF  WRK-FIM-CLIENDUP         EQUAL 'S'
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*          ARQUIVO CLIENDUP VAZIO            *'
               DISPLAY '*                                            *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO           *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE WRK-MENSA01         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           ELSE
               MOVE WRK-CHV-SUMARIZA    TO WRK-CHV-ANTERIOR
               MOVE REG-INTERMED-SACADO TO WRK-REG-INTERMED-SACADO
           END-IF.

           PERFORM 220000-LER-PERIDECE.

           IF  WRK-FS-PERIDECE          EQUAL '10'
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*           ARQUIVO PERIDECE VAZIO           *'
               DISPLAY '*                                            *'
               DISPLAY '*          PROCESSAMENTO  CANCELADO          *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE WRK-MENSA02         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 220000-LER-PERIDECE  UNTIL
               WRK-FS-PERIDECE          EQUAL '10' OR
               SLIG00-CD-TP-PROCESSA    EQUAL 2.

           IF  WRK-FS-PERIDECE          EQUAL '10'
               DISPLAY '****************** SLIG6102 ******************'
               DISPLAY '*                                            *'
               DISPLAY '*      SLIG00-CD-TP-PROCESSA IGUAL A 2       *'
               DISPLAY '*               NAO ENCONTRADO               *'
               DISPLAY '*          PROCESSAMENTO  CANCELADO          *'
               DISPLAY '*                                            *'
               DISPLAY '****************** SLIG6102 ******************'
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE WRK-MENSA03         TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 230000-LER-CADACLIE.

           IF WRK-FS-CADACLIE   EQUAL  'S'  AND
              ACU-LIDOS-CADACLIE    NOT GREATER ZEROS
               DISPLAY '***************** SLIG6102 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CADACLIE VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6102 *****************'
           END-IF.

           PERFORM 240000-LER-CADACBCL.

CNPJEP     IF WRK-FS-CADACBCL   EQUAL  '10' AND
              ACU-LIDOS-CADACBCL    NOT GREATER ZEROS
               DISPLAY '***************** SLIG6102 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CADACBCL VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO  CANCELADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6102 *****************'
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-CLIENDUP             SECTION.
      *----------------------------------------------------------------*

           MOVE 'BRAD0300'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-CLIENDUP
                                             REG-INTERMED-SACADO.

           IF  RETURN-CODE             EQUAL 4
               MOVE 'S'                TO WRK-FIM-CLIENDUP
               GO TO 210000-99-FIM
           END-IF.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'CLIENDUP'         TO WRK-NOME-ARQ
               MOVE WRK-LEITURA        TO WRK-OPERACAO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               PERFORM 211000-TRATAR-COD-RETORNO
           END-IF.

      *--  FORMATA CHAVES DE SUMARIZACAO  -----------------------------*

CNPJAN     MOVE SLIG97-CGC-CPF-CLIENTE-ST  TO WRK-CH-CGC-CPF-CLI.
.          IF SLIG97-CGC-CPF-CLIENTE-ST EQUAL SPACES OR LOW-VALUES
.             ADD  1                       TO ACU-CGC-NAO-POP-SLIG97
.             MOVE SLIG97-CGC-CPF-CLIENTE  TO WRK-CNPJ-9-S
.             MOVE WRK-CNPJ-9-X            TO WRK-CH-CGC-CPF-CLI
.          END-IF
.          MOVE SLIG97-FILIAL-CLIENTE-ST   TO WRK-CH-FILIAL-CLI.
.          IF SLIG97-FILIAL-CLIENTE-ST  EQUAL SPACES OR LOW-VALUES
.             ADD  1                       TO ACU-CGC-NAO-POP-SLIG97
.             MOVE SLIG97-FILIAL-CLIENTE   TO WRK-FILIAL-5-S
.             MOVE WRK-FILIAL-5-X          TO WRK-CH-FILIAL-CLI
.          END-IF
.          MOVE SLIG97-CONTROLE-CLIENTE-ST TO WRK-CH-CONTROLE-CLI.
.          IF SLIG97-CONTROLE-CLIENTE-ST  EQUAL ZEROS
.             ADD  1                       TO ACU-CGC-NAO-POP-SLIG97
.             MOVE SLIG97-CONTROLE-CLIENTE TO WRK-CH-CONTROLE-CLI
.          END-IF
.          MOVE SLIG97-CGC-CPF-SACADO-ST   TO WRK-CH-CGC-CPF-SAC.
.          IF SLIG97-CGC-CPF-SACADO-ST  EQUAL SPACES OR LOW-VALUES
.             ADD  1                       TO ACU-CGC-NAO-POP-SLIG97
.             MOVE SLIG97-CGC-CPF-SACADO   TO WRK-CNPJ-9-S
.             MOVE WRK-CNPJ-9-X            TO WRK-CH-CGC-CPF-SAC
.          END-IF
.          MOVE SLIG97-FILIAL-SACADO-ST    TO WRK-CH-FILIAL-SAC.
QG
QG         IF SLIG97-FILIAL-SACADO      NOT NUMERIC
QG            DISPLAY 'SLIG97-FILIAL = ' SLIG97-FILIAL-SACADO
QG            DISPLAY 'SLIG97-REGISTRO = ' REG-INTERMED-SACADO
QG            MOVE ZEROS                   TO SLIG97-FILIAL-SACADO
QG         END-IF
QG
.          IF SLIG97-FILIAL-SACADO-ST   EQUAL SPACES OR LOW-VALUES
.             ADD  1                       TO ACU-CGC-NAO-POP-SLIG97
.             MOVE SLIG97-FILIAL-SACADO    TO WRK-FILIAL-5-S
.             MOVE WRK-FILIAL-5-X          TO WRK-CH-FILIAL-SAC
.          END-IF
.          MOVE SLIG97-CONTROLE-SACADO-ST  TO WRK-CH-CONTROLE-SAC.
AQUI       IF SLIG97-CONTROLE-SACADO-ST EQUAL ZEROS
.             ADD  1                       TO ACU-CGC-NAO-POP-SLIG97
.             MOVE SLIG97-CONTROLE-SACADO  TO WRK-CH-CONTROLE-SAC
CNPJAN     END-IF
           MOVE SLIG97-CTPO-CART           TO WRK-CH-CTPO-CART.

      *--  GUARDA BCO AGE E CTA DO CLIENTE QDO DIFERENTE DE ZEROS  ----*

           IF (SLIG97-BCO-CLIENTE      NOT EQUAL ZEROS    AND
               WRK-SLIG97-BCO-CLIENTE      EQUAL ZEROS )  AND
              (SLIG97-AGENCIA-CLIENTE  NOT EQUAL ZEROS    AND
               WRK-SLIG97-AGENCIA-CLIENTE  EQUAL ZEROS )  AND
              (SLIG97-CONTA-CLIENTE    NOT EQUAL ZEROS    AND
               WRK-SLIG97-CONTA-CLIENTE    EQUAL ZEROS)

               MOVE SLIG97-BCO-CLIENTE    TO  WRK-SLIG97-BCO-CLIENTE
               MOVE SLIG97-AGENCIA-CLIENTE TO WRK-SLIG97-AGENCIA-CLIENTE
               MOVE SLIG97-CONTA-CLIENTE  TO  WRK-SLIG97-CONTA-CLIENTE
           END-IF.

      *--  GUARDA BCO AGE E CTA DO SACADO QDO DIFERENTE DE ZEROS  -----*

           IF  (SLIG97-BCO-SACADO       NOT EQUAL ZEROS    AND
                WRK-SLIG97-BCO-SACADO       EQUAL ZEROS )  AND
               (SLIG97-AGENCIA-SACADO   NOT EQUAL ZEROS    AND
                WRK-SLIG97-AGENCIA-SACADO   EQUAL ZEROS )  AND
               (SLIG97-CONTA-SACADO     NOT EQUAL ZEROS    AND
                WRK-SLIG97-CONTA-SACADO     EQUAL ZEROS)

                MOVE SLIG97-BCO-SACADO     TO  WRK-SLIG97-BCO-SACADO
                MOVE SLIG97-AGENCIA-SACADO TO  WRK-SLIG97-AGENCIA-SACADO
                MOVE SLIG97-CONTA-SACADO   TO  WRK-SLIG97-CONTA-SACADO
           END-IF.

           ADD 1                       TO ACU-LIDOS-CLIENDUP.

      *---->       CALCULA DT DE CARENCIA A PARTIR DA DT DE VENCIMENTO

           MOVE    SLIG97-DT-VENCTO-PROD   TO  WRK-DT-VENCTO-PROD-AUX.
           PERFORM 221000-CALCULAR-DT-CARENCIA.

      *--  FORMATA NOME DO SACADO A PARTIR DA ORIGEM  -----------------*

           IF WRK-CHV-SUMARIZA         EQUAL WRK-CHV-ANTERIOR AND
              WRK-CD-ORIG-NM-SAC       EQUAL 02  AND
              SLIG97-CD-ORIG-NM-SAC    EQUAL 01

              MOVE SLIG97-NOME-SACADO    TO WRK-NOME-SACADO
              MOVE 01                    TO WRK-CD-ORIG-NM-SAC

           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-TRATAR-COD-RETORNO       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************** SLIG6102 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '*        ERRO ' WRK-OPERACAO
                                              ' DO ARQUIVO       *'.
           DISPLAY '*                  ' WRK-NOME-ARQ
                                              '                  *'.
           DISPLAY '*              FILE STATUS = ' WRK-RETURN-CODE
                                                  '              *'.
           DISPLAY '*                                            *'.
           DISPLAY '****************** SLIG6102 ******************'.

           CALL 'ILBOABN0'             USING WRK-ABEND.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       220000-LER-PERIDECE             SECTION.
      *----------------------------------------------------------------*

           READ PERIDECE.

           IF  WRK-FS-PERIDECE         EQUAL '10'
               GO TO 220000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-PERIDECE.

           IF  SLIG00-CD-TP-PROCESSA   EQUAL 2
               MOVE SLIG00-DT-INI-PER-ATU TO WRK-DT-INI-PER-R
               MOVE SLIG00-DT-FIM-PER-ATU TO WRK-DT-FIM-PER-R
           END-IF.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       221000-CALCULAR-DT-CARENCIA     SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DT-VENCTO-PROD-AUX TO WRK-1640-DATA-ENTRADA-N.
           MOVE 04                     TO WRK-1640-FORMATO-ENTRADA
                                          WRK-1640-FORMATO-SAIDA.
4SYS01******OVE 'DC'                   TO WRK-1640-TIPO-INCREMENTO.
4SYS01******OVE 10                     TO WRK-1640-INCREMENTO.
4SYS01     MOVE 'DU'                   TO WRK-1640-TIPO-INCREMENTO.
4SYS01     MOVE 3                      TO WRK-1640-INCREMENTO.

           CALL 'POOL1640'             USING WRK-1640-DATA-ENTRADA
                                             WRK-1640-FORMATO-ENTRADA
                                             WRK-1640-TIPO-INCREMENTO
                                             WRK-1640-INCREMENTO
                                             WRK-1640-FORMATO-SAIDA
                                             WRK-1640-DATA-SAIDA
                                             WRK-1640-MENSAGEM.

           IF RETURN-CODE              EQUAL ZEROS
              MOVE WRK-1640-DATA-SAIDA TO WRK-DT-VCT-PRD-CAR-R
           ELSE
              MOVE RETURN-CODE         TO WRK-RET-COD-1640
              DISPLAY '****************** SLIG6102 ******************'
              DISPLAY '*                                            *'
              DISPLAY '*         ERRO NO ACESSO A POOL1640          *'
              DISPLAY '*              RETURN-CODE = ' WRK-RET-COD-1640
                                                     '              *'
              DISPLAY '*          PROCESSAMENTO CANCELADO           *'
              DISPLAY '*                                            *'
              DISPLAY '****************** SLIG6102 ******************'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'POOL1640'          TO ERR-MODULO
              MOVE WRK-MENSA04         TO ERR-TEXTO
              PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       221000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       230000-LER-CADACLIE             SECTION.
      *----------------------------------------------------------------*

           MOVE 'BRAD0300'             TO WRK-MODULO

           CALL WRK-MODULO  USING  WRK-CADACLIE  REG-DEVE17.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           IF  RETURN-CODE     EQUAL   04
               MOVE  'S'               TO WRK-FS-CADACLIE
CNPJEP         MOVE HIGH-VALUES        TO WRK-CPFCGC-DEVE
               GO TO 230000-99-FIM
           END-IF.

           PERFORM 231000-TRATAR-COD-RETORNO.

      *--  FORMATA CHAVE PARA CADACLIE  -------------------------------*

CAPJAN*   -MOVE DEVE17-PRINCIPAL    TO WRK-VALOR-HEXA-09-S.
CNPJAN*   -MOVE WRK-VALOR-HEXA-09-X TO WRK-CPFCGC-DEVE.
CNPJAN*   -MOVE DEVE17-FILIAL       TO WRK-VALOR-HEXA-09-S.
CNPJAN*   -MOVE WRK-VALOR-HEXA-05-X TO WRK-FILIAL-DEVE.
CNPJAN*   -MOVE DEVE17-CTLE         TO WRK-CONTROLE-DEVE.
CNPJAN     MOVE DEVE17-PRINCIPAL-ST TO WRK-CPFCGC-DEVE.
.          IF DEVE17-PRINCIPAL-ST   EQUAL SPACES OR LOW-VALUES
.             ADD  1                TO ACU-CGC-NAO-POP-DEVE17
.             MOVE DEVE17-PRINCIPAL TO WRK-CNPJ-9-S
.             MOVE WRK-CNPJ-9-X     TO WRK-CPFCGC-DEVE
.          END-IF
.          MOVE DEVE17-FILIAL-ST    TO WRK-FILIAL-DEVE.
.          IF DEVE17-FILIAL-ST      EQUAL SPACES OR LOW-VALUES
.             ADD  1                TO ACU-CGC-NAO-POP-DEVE17
.             MOVE DEVE17-FILIAL    TO WRK-FILIAL-5-S
.             MOVE WRK-FILIAL-5-X   TO WRK-FILIAL-DEVE
.          END-IF
.          MOVE DEVE17-CTLE-ST      TO WRK-CONTROLE-DEVE.
.          IF DEVE17-CTLE-ST        EQUAL ZEROS
.             ADD  1                TO ACU-CGC-NAO-POP-DEVE17
.             MOVE DEVE17-CTLE-ST   TO WRK-CONTROLE-DEVE
CNPJAN     END-IF

      *--  ACUMULA LIDOS ----------------------------------------------*

           ADD 1                   TO ACU-LIDOS-CADACLIE.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       231000-TRATAR-COD-RETORNO       SECTION.
      *----------------------------------------------------------------*

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               DISPLAY '***************** SLIG6102 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*        ERRO NA LEITURA DO ARQUIVO        *'
               DISPLAY '*           CADACLIE PELA BRAD0300         *'
               DISPLAY '*                                          *'
               DISPLAY '*             RETURN-CODE = ' WRK-RETURN-CODE
                                                     '             *'
               DISPLAY '*                                          *'
               DISPLAY '***************** SLIG6102 *****************'
           END-IF.

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       240000-LER-CADACBCL             SECTION.
      *----------------------------------------------------------------*

           READ CADACBCL.

           IF  WRK-FS-CADACBCL         EQUAL '10'
CNPJEP         MOVE HIGH-VALUES        TO WRK-CPFCGC-CBCL
CNPJEP                                    WRK-FILIAL-CBCL
IBM001         MOVE 99                 TO WRK-CONTROLE-CBCL
               GO TO 240000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 115000-TESTAR-FS-CADACBCL.

      *--  FORMATA CHAVE PARA CADACBCL  -------------------------------*

           MOVE SLIG20-NEGOC-CGC-CPF-ST     TO WRK-CPFCGC-CBCL.
           MOVE SLIG20-NEGOC-FIL-CGC-CPF-ST TO WRK-FILIAL-CBCL.
           MOVE SLIG20-NEGOC-CTR-CGC-CPF    TO WRK-CONTROLE-CBCL.

      *--  ACUMULA LIDOS ----------------------------------------------*

           ADD 1                   TO ACU-LIDOS-CADACBCL.

      *----------------------------------------------------------------*
       240000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-SUMARIZA         NOT EQUAL WRK-CHV-ANTERIOR

              PERFORM 310000-GRAVAR-CLIENUNI

              MOVE WRK-CHV-SUMARIZA    TO WRK-CHV-ANTERIOR

              MOVE REG-INTERMED-SACADO TO WRK-REG-INTERMED-SACADO

              MOVE ZEROS               TO WRK-VANTES-PER-NAO-PG
                                          WRK-VVCTO-PER-PG
                                          WRK-VVCTO-PER-NAO-PG
                                          WRK-VVCTO-PER-BXADO
                                          WRK-VVCTO-APOS-PER-PG
                                          WRK-VVCTO-APOS-PER-NAO-PG
                                          WRK-VVCTO-APOS-PER-BXADO
                                          WRK-VTOT-PRROG
                                          WRK-VTOT-ABTMT
           END-IF.

           PERFORM 320000-CONVERTER-DECIMAIS.

           PERFORM 330000-ACUMULAR-VALORES.

           PERFORM 210000-LER-CLIENDUP.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-GRAVAR-CLIENUNI          SECTION.
      *----------------------------------------------------------------*

           PERFORM 311000-FORMATAR-CPOS.

           IF ZEROS      EQUAL SLIG02-VANTES-PER-NAO-PG     AND
                               SLIG02-VVCTO-PER-PG          AND
                               SLIG02-VVCTO-PER-NAO-PG      AND
                               SLIG02-VVCTO-PER-BXADO       AND
                               SLIG02-VVCTO-APOS-PER-PG     AND
                               SLIG02-VVCTO-APOS-PER-NAO-PG AND
                               SLIG02-VVCTO-APOS-PER-BXADO  AND
                               SLIG02-VTOT-PRROG            AND
                               SLIG02-VTOT-ABTMT
              ADD   1    TO    ACU-DESPR-CLIENDUP

              IF ACU-DESPR-CLIENDUP LESS 101
                 DISPLAY
                 '->DESPREZADO - CLI: ' SLIG02-CGC-CPF-CLIENTE-ST
                 '/' SLIG02-FILIAL-CLIENTE-ST
                 '-' SLIG02-CONTROLE-CLIENTE-ST
                 ' SAC: ' SLIG02-CGC-CPF-SACADO-ST
                 '/' SLIG02-FILIAL-SACADO-ST
                 '-' SLIG02-CONTROLE-SACADO-ST
                 ' CART: '   SLIG02-CTPO-CART
                 ' CCLI-COBR: '  SLIG02-CCLI-COBR
              END-IF

              GO TO 310000-99-FIM
           END-IF.

           WRITE REG-INTERMEDIARIO-TITULO.

           PERFORM 113000-TESTAR-FS-CLIENUNI.

           ADD 1                       TO ACU-GRAVA-CLIENUNI.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       311000-FORMATAR-CPOS            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE REG-INTERMEDIARIO-TITULO.

      *--  FORMATA DADOS DO CLIENTE  ----------------------------------*

FS0617     MOVE WRK-BCO-CLIENTE              TO SLIG02-BCO-CLIENTE.
'          MOVE WRK-AGENCIA-CLIENTE          TO SLIG02-AGENCIA-CLIENTE.
FS0617     MOVE WRK-CONTA-CLIENTE            TO SLIG02-CONTA-CLIENTE.

           IF   SLIG02-BCO-CLIENTE         EQUAL ZEROS     AND
                SLIG02-AGENCIA-CLIENTE NOT EQUAL ZEROS     AND
                SLIG02-CONTA-CLIENTE   NOT EQUAL ZEROS     THEN
                MOVE   237                   TO SLIG02-BCO-CLIENTE
           END-IF.

           MOVE WRK-CH-CGC-CPF-CLI-ANT   TO SLIG02-CGC-CPF-CLIENTE-ST.
           MOVE WRK-CH-FILIAL-CLI-ANT    TO SLIG02-FILIAL-CLIENTE-ST.
           MOVE WRK-CH-CONTROLE-CLI-ANT  TO SLIG02-CONTROLE-CLIENTE-ST.

CNPJAN     IF  (WRK-CH-FILIAL-CLI-ANT     EQUAL '00000' OR
CNPJAN          WRK-CH-FILIAL-CLI-ANT     EQUAL SPACES ) AND
CNPJAN         WRK-CH-CGC-CPF-CLI-ANT    NOT EQUAL '000000000' AND
CNPJAN         WRK-CH-CGC-CPF-CLI-ANT    NOT EQUAL SPACES
               MOVE 1                    TO SLIG02-TP-PESS-CLIENTE
           ELSE
               MOVE 2                    TO SLIG02-TP-PESS-CLIENTE

           END-IF.



      *--  FORMATA DADOS DO SACADO  -----------------------------------*

           MOVE WRK-NOME-SACADO               TO SLIG02-NM-SACADO.

FS0617     MOVE WRK-BCO-SACADO                TO SLIG02-BCO-SACADO.
'          MOVE WRK-AGENCIA-SACADO            TO SLIG02-AGENCIA-SACADO.
FS0617     MOVE WRK-CONTA-SACADO              TO SLIG02-CONTA-SACADO.

           IF   SLIG02-BCO-SACADO          EQUAL ZEROS     AND
                SLIG02-AGENCIA-SACADO  NOT EQUAL ZEROS     AND
                SLIG02-CONTA-SACADO    NOT EQUAL ZEROS     THEN
                MOVE   237                    TO SLIG02-BCO-SACADO
           END-IF.

           MOVE WRK-CH-CGC-CPF-SAC-ANT  TO SLIG02-CGC-CPF-SACADO-ST.
           MOVE WRK-CH-FILIAL-SAC-ANT   TO SLIG02-FILIAL-SACADO-ST.
           MOVE WRK-CH-CONTROLE-SAC-ANT TO SLIG02-CONTROLE-SACADO-ST.

      *----DISPLAY-'* SLIG02-TP-PESS-CLIENTE...: '
      *                                  SLIG02-TP-PESS-CLIENTE
      *----DISPLAY-'* SLIG02-CGC-CPF-SACADO-ST.: '
      *                                  SLIG02-CGC-CPF-SACADO-ST
      *----DISPLAY-'* SLIG02-FILIAL-SACADO-ST..: '
      *                                  SLIG02-FILIAL-SACADO-ST
      *----DISPLAY-'* SLIG02-CONTROLE-SACADO-ST: '
      *                                  SLIG02-CONTROLE-SACADO-ST

CNPJAN     IF (WRK-CH-FILIAL-SAC-ANT       EQUAL '00000' OR
CNPJAN         WRK-CH-FILIAL-SAC-ANT       EQUAL SPACES  )AND
CNPJAN         WRK-CH-CGC-CPF-SAC-ANT  NOT EQUAL '000000000' AND
CNPJAN         WRK-CH-CGC-CPF-SAC-ANT  NOT EQUAL SPACES
               MOVE 1                  TO SLIG02-TP-PESS-SACADO
           ELSE
               MOVE 2                  TO SLIG02-TP-PESS-SACADO
           END-IF.

      *--  FORMATA DADOS DO TITULO  -----------------------------------*

           MOVE WRK-CH-CTPO-CART-ANT   TO SLIG02-CTPO-CART.

           MOVE WRK-CCLI-COBR          TO SLIG02-CCLI-COBR.

           MOVE WRK-DT-INI-PER-R
                                       TO SLIG02-DT-INICIO-PER.
           MOVE WRK-DT-FIM-PER-R
                                       TO SLIG02-DT-FINAL-PER.
           MOVE WRK-VANTES-PER-NAO-PG  TO SLIG02-VANTES-PER-NAO-PG.
           MOVE WRK-VVCTO-PER-PG       TO SLIG02-VVCTO-PER-PG.
           MOVE WRK-VVCTO-PER-NAO-PG   TO SLIG02-VVCTO-PER-NAO-PG.

           MOVE WRK-VVCTO-PER-BXADO    TO SLIG02-VVCTO-PER-BXADO.
           MOVE WRK-VVCTO-APOS-PER-PG  TO SLIG02-VVCTO-APOS-PER-PG.
           MOVE WRK-VVCTO-APOS-PER-NAO-PG
                                       TO SLIG02-VVCTO-APOS-PER-NAO-PG.
           MOVE WRK-VVCTO-APOS-PER-BXADO
                                       TO SLIG02-VVCTO-APOS-PER-BXADO.
           MOVE WRK-VTOT-PRROG         TO SLIG02-VTOT-PRROG.
           MOVE WRK-VTOT-ABTMT         TO SLIG02-VTOT-ABTMT.

           MOVE ZEROS                  TO SLIG02-CGC-GRUPO-CLIENTE
                                          SLIG02-CD-DIR-CLIENTE
                                          SLIG02-CD-BCO-GER-REGIONAL
                                          SLIG02-GER-REGIONAL
                                          SLIG02-CGC-GRUPO-SACADO
                                          SLIG02-CD-DIR-SACADO.


      *--  FORMATA GRUPO DO CLIENTE  ----------------------------------*

CNPJAN     IF  SLIG02-CGC-CPF-CLIENTE-ST NOT EQUAL '000000000'
CNPJAN     AND SLIG02-CGC-CPF-CLIENTE-ST NOT EQUAL SPACES
CNPJAN     AND SLIG02-CGC-CPF-CLIENTE-ST NOT EQUAL LOW-VALUES
               PERFORM 420000-OBTER-GRUPO-CLIENTE
           END-IF.

      *--  FORMATA NOME A PARTIR DO CADACBCL  -------------------------*

CNPJAN     IF  SLIG02-CGC-CPF-CLIENTE-ST NOT EQUAL '000000000'
CNPJAN     AND SLIG02-CGC-CPF-CLIENTE-ST NOT EQUAL SPACES
CNPJAN     AND SLIG02-CGC-CPF-CLIENTE-ST NOT EQUAL LOW-VALUES

               IF WRK-CHV-ULTNOME NOT EQUAL WRK-CH-CLIENTE-ANT

                  MOVE 'N'          TO  WRK-FLAG-CADACBCL
                  MOVE SPACES       TO  WRK-NOME-CLIENTE

                  PERFORM 340000-FORMATAR-NOME
                          UNTIL WRK-FLAG-CADACBCL = 'S'

                  MOVE WRK-CH-CLIENTE-ANT TO WRK-CHV-ULTNOME
               END-IF

               MOVE WRK-NOME-CLIENTE                TO
                    SLIG02-NM-CLIENTE
           ELSE
               MOVE 'CLIENTE COM CNPJ/CPF ZERADO'   TO
                    SLIG02-NM-CLIENTE
           END-IF.

      *--  ZERA VARIAVEIS DE GUARDA  ----------------------------------*

           MOVE ZEROS                      TO WRK-SLIG97-BCO-CLIENTE
                                              WRK-SLIG97-AGENCIA-CLIENTE
                                              WRK-SLIG97-CONTA-CLIENTE
                                              WRK-SLIG97-BCO-SACADO
                                              WRK-SLIG97-AGENCIA-SACADO
                                              WRK-SLIG97-CONTA-SACADO.

      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       320000-CONVERTER-DECIMAIS       SECTION.
      *----------------------------------------------------------------*

           COMPUTE     WRK-VL-PRINC-PROD-CONV =
                       SLIG97-VL-PRINC-PROD / 100.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       330000-ACUMULAR-VALORES         SECTION.
      *----------------------------------------------------------------*

4SYS01*------ MOVIMENTADO A ROTINA DE VERIFICACAO DE TITULOS ----------*
4SYS01*------ PRORROGADOS PARA O INICIO DAS VALIDACOES PARA  ----------*
4SYS01*------ EVITAR ACUMULO DE VALORES NAS DEMAIS SITUACOES ----------*

      *   8_  ACUMULA  VTOT-PRROG
      *                TITULOS PRORROGADOS COM DATA DE SOLICITACAO NO
      *                PERIODO, INDEPENDENTE DO VENCIMENTO

ISF        MOVE WRK-DT-INI-PER-R          TO WRK-DT-INI-PER-R-9
ISF        MOVE WRK-DT-FIM-PER-R          TO WRK-DT-FIM-PER-R-9

           IF (SLIG97-CD-OCO-REM-L     EQUAL 006 OR 031)             AND
ISF************SLIG97-DT-ENTRADA-ORIGEM  NOT LESS WRK-DT-INI-PER-R   AND
               SLIG97-DT-ENTRADA-ORIGEM  NOT LESS WRK-DT-INI-PER-R-9 AND
               SLIG97-DT-ENTRADA-ORIGEM  NOT GREATER
ISF************                              WRK-DT-FIM-PER-R
                                             WRK-DT-FIM-PER-R-9

               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VTOT-PRROG

      *------  PRORROGACOES COM VENCIMENTO-ANTERIOR NO PERIODO --------*
      *------  EH SOMADO NO VENCIMENTOS DA CARTEIRA E POR      --------*
      *------  CONSEQUENCIA NO TOTAL DA CARTEIRA               --------*

               IF (SLIG97-DT-VENCTO-ANTERIOR NOT LESS
ISF************    WRK-DT-INI-PER-R              AND
ISF                WRK-DT-INI-PER-R-9            AND
                   SLIG97-DT-VENCTO-ANTERIOR NOT GREATER
ISF************    WRK-DT-FIM-PER-R)             AND
ISF                WRK-DT-FIM-PER-R-9)           AND
                  (SLIG97-DT-VENCTO-PROD         GREATER
ISF************    WRK-DT-FIM-PER-R)
ISF                WRK-DT-FIM-PER-R-9)

                  ADD      WRK-VL-PRINC-PROD-CONV TO
                           WRK-VVCTO-PER-NAO-PG

      *---------  FOI NECESSARIA A SUBTRACAO DO ITEM   6  ------------*
      *---------  VVCTO-APOS-PER-NAO-PG PARA QUE O TOTAL  ------------*
      *---------  DA CARTEIRA NAO SEJA SOMADO DUAS VEZES  ------------*

                  SUBTRACT WRK-VL-PRINC-PROD-CONV  FROM
                           WRK-VVCTO-APOS-PER-NAO-PG

               END-IF
4SYS01         GO TO 330000-99-FIM
           END-IF.

      *   1_  ACUMULA  VANTES-PER-NAO-PG
      *                TITULOS EM ABERTO OU BAIXADOS COM VENCIMENTO
      *                ANTERIOR AO PERIODO DE REFERENCIA

           IF  SLIG97-DT-VENCTO-PROD   LESS WRK-DT-INI-PER-R         AND
             ((SLIG97-DT-PAGTO-PROD    EQUAL ZEROS                   AND
               SLIG97-VL-PAGO-PROD     EQUAL ZEROS                )  OR
              (SLIG97-DT-PAGTO-PROD    EQUAL ZEROS                AND
               SLIG97-CD-OCO-REM-L     EQUAL 002                     AND
              (SLIG97-CD-MOT-REM-SIS   NOT EQUAL 061 AND 062 AND 067 AND
                                                 066) ))

               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VANTES-PER-NAO-PG
           END-IF.

      *   2_  ACUMULA  VVCTO-PER-PG
      *                TITULOS COM DATA DE VENCIMENTO NO PERIODO DE PAGO
      *                DENTRO DA CARENCIA NAO ANTECIPADO

           IF  SLIG97-DT-VENCTO-PROD   NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-VENCTO-PROD   NOT GREATER
                                           WRK-DT-FIM-PER-R          AND
               SLIG97-DT-PAGTO-PROD    NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-PAGTO-PROD    NOT GREATER
                                           WRK-DT-VCT-PRD-CAR-N      AND
               SLIG97-VL-PAGO-PROD     NOT EQUAL ZEROS               AND
               SLIG97-DT-PAGTO-PROD    NOT LESS
                                           SLIG97-DT-VENCTO-PROD     AND
               SLIG97-CD-OCO-REM-L     EQUAL 002                     AND
              (SLIG97-CD-MOT-REM-SIS   EQUAL 061 OR 062 OR 066 OR 067)

               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VVCTO-PER-PG
           END-IF.

      *   3_  ACUMULA  VVCTO-PER-NAO-PG
      *                TITULOS PENDENTES NO PERIODO E PAGOS FORA DA
      *                CARENCIA

           IF  SLIG97-DT-VENCTO-PROD   NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-VENCTO-PROD   NOT GREATER
                                       WRK-DT-FIM-PER-R              AND
             ((SLIG97-DT-PAGTO-PROD    EQUAL ZEROS                   AND
               SLIG97-VL-PAGO-PROD     EQUAL ZEROS   )               OR
              (SLIG97-VL-PAGO-PROD     NOT EQUAL ZEROS               AND
               SLIG97-DT-PAGTO-PROD    GREATER WRK-DT-VCT-PRD-CAR-N  AND
               SLIG97-CD-OCO-REM-L     EQUAL 002                     AND
              (SLIG97-CD-MOT-REM-SIS   EQUAL 061 OR 062 OR 066 OR 067)))

               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VVCTO-PER-NAO-PG
           END-IF.

      *   4_  ACUMULA  VVCTO-PER-BXADO
      *                TITULOS COM DATA DE VENCIMENTO NO PERIODO E
      *                BAIXADOS NAO ANTECIPADO

           IF  SLIG97-DT-VENCTO-PROD   NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-VENCTO-PROD   NOT GREATER
                                       WRK-DT-FIM-PER-R              AND
               SLIG97-DT-ENTRADA-ORIGEM NOT LESS
                                               SLIG97-DT-VENCTO-PROD AND
               SLIG97-CD-OCO-REM-L     EQUAL 002                     AND
              (SLIG97-CD-MOT-REM-SIS   NOT EQUAL 061 AND 062 AND 067 AND
                                                 066)

               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VVCTO-PER-BXADO
           END-IF.

      *   5_  ACUMULA  VVCTO-APOS-PER-PG (ANTECIPADOS)
      *                TITULOS PAGOS NO PERIODO E ATENCIPADOS

           IF  SLIG97-VL-PAGO-PROD     NOT EQUAL ZEROS               AND
               SLIG97-CD-OCO-REM-L     EQUAL 002                     AND
              (SLIG97-CD-MOT-REM-SIS   EQUAL 061 OR 062 OR 066 OR 067)
                                                                     AND
               SLIG97-DT-PAGTO-PROD    NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-PAGTO-PROD    NOT GREATER
                                       WRK-DT-FIM-PER-R              AND
               SLIG97-DT-PAGTO-PROD    LESS     SLIG97-DT-VENCTO-PROD
               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VVCTO-APOS-PER-PG
           END-IF.

      *   6_  ACUMULA  VVCTO-APOS-PER-NAO-PG
      *                TITULOS PENDENTES EM ABERTO COM VENCIMENTO FUTURO

           IF  SLIG97-DT-VENCTO-PROD   GREATER WRK-DT-FIM-PER-R      AND
               SLIG97-DT-PAGTO-PROD    EQUAL ZEROS                   AND
               SLIG97-VL-PAGO-PROD     EQUAL ZEROS
               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VVCTO-APOS-PER-NAO-PG
           END-IF.

      *   7_  ACUMULA  VVCTO-APOS-PER-BXADO (ANTECIPADOS)
      *                TITULOS BAIXADOS NO PERIODO E VENCIMENTO FUTURO
      *                ATENCIPADOS OU NAO

           IF  SLIG97-DT-VENCTO-PROD   GREATER  WRK-DT-FIM-PER-R     AND
               SLIG97-DT-ENTRADA-ORIGEM   LESS
                                       SLIG97-DT-VENCTO-PROD         AND
               SLIG97-CD-OCO-REM-L     EQUAL 002                     AND
              (SLIG97-CD-MOT-REM-SIS   NOT EQUAL 061 AND 062 AND 067 AND
                                                 066)

               ADD WRK-VL-PRINC-PROD-CONV TO WRK-VVCTO-APOS-PER-BXADO
           END-IF.

      *   9_  ACUMULA  VTOT-ABTMT
      *                TITULOS PAGOS DENTRO DO PERIODO ANTECIPADOS E
      *                PAGOS DENTRO DA CARENCIA NAO ANTECIPADOS,
      *                CUJA A DIFERENCA ENTRE O VALOR DEVIDO E O VALOR
      *                PAGO SEJA A MENOR

           MOVE    SLIG97-VL-PAGO-PROD   TO WRK-VL-PG-PROD.

           COMPUTE WRK-OSCILACAO       = WRK-VL-PRINC-PROD-CONV
                                       - WRK-VL-PG-PROD-R.

           IF  WRK-OSCILACAO           GREATER ZEROS                 AND
               SLIG97-CD-OCO-REM-L     EQUAL 002                     AND
              (SLIG97-CD-MOT-REM-SIS   EQUAL 061 OR 062 OR 066 OR 067)
                                                                     AND
             ((SLIG97-DT-VENCTO-PROD   NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-VENCTO-PROD   NOT GREATER
                                                WRK-DT-FIM-PER-R     AND
               SLIG97-DT-PAGTO-PROD    NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-PAGTO-PROD    NOT GREATER
                                                WRK-DT-VCT-PRD-CAR-N AND
               SLIG97-DT-PAGTO-PROD    NOT LESS SLIG97-DT-VENCTO-PROD )
                                                                     OR
              (SLIG97-DT-PAGTO-PROD    NOT LESS WRK-DT-INI-PER-R     AND
               SLIG97-DT-PAGTO-PROD    NOT GREATER
                                                WRK-DT-FIM-PER-R     AND
               SLIG97-DT-PAGTO-PROD    LESS     SLIG97-DT-VENCTO-PROD ))

               ADD WRK-OSCILACAO       TO WRK-VTOT-ABTMT
           END-IF.

      *----------------------------------------------------------------*
       330000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       340000-FORMATAR-NOME            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CH-CLIENTE-ANT     EQUAL  WRK-CHV-CBCL

               MOVE 'S'                    TO WRK-FLAG-CADACBCL
               MOVE SLIG20-NEGOC-ICLI-COBR TO WRK-NOME-CLIENTE
               ADD  1                      TO ACU-ACHOU-CADACBCL

           ELSE
              IF  WRK-CH-CLIENTE-ANT     LESS  WRK-CHV-CBCL
                  MOVE 'N'          TO  WRK-FLAG-CADACLIE
                  MOVE SPACES       TO  WRK-NOME-CLIENTE
                  ADD  1            TO  ACU-NAO-ACHOU-CADACBCL

                  PERFORM 345000-PROCURAR-NOME-NO-CLIE
                          UNTIL WRK-FLAG-CADACLIE = 'S'
              ELSE
                  PERFORM 240000-LER-CADACBCL
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       340000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       345000-PROCURAR-NOME-NO-CLIE    SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CH-CLIENTE-ANT     EQUAL  WRK-CHV-DEVE

               MOVE 'S'                      TO WRK-FLAG-CADACLIE
                                                WRK-FLAG-CADACBCL
               MOVE DEVE17-NOME-CLIE         TO WRK-NOME-CLIENTE
               ADD  1                        TO ACU-ACHOU-CADACLIE

           ELSE
              IF  WRK-CH-CLIENTE-ANT     LESS  WRK-CHV-DEVE

                  MOVE 'S'             TO    WRK-FLAG-CADACLIE
                                             WRK-FLAG-CADACBCL
                  MOVE 'NOME DO CLIENTE NAO ENCONTRADO'
                                       TO    WRK-NOME-CLIENTE
                  ADD  1               TO    ACU-NAO-ACHOU-CADACLIE

                  PERFORM 350000-GRAVAR-ARQERROS
              ELSE
                  PERFORM 230000-LER-CADACLIE

              END-IF
           END-IF.

      *----------------------------------------------------------------*
       345000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       350000-GRAVAR-ARQERROS          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE REG-ERRO.

           MOVE  SLIG02-BCO-CLIENTE       TO SLIG22-BANCO.
           MOVE  SLIG02-CTPO-CART         TO SLIG22-CARTEIRA.
           MOVE  SLIG02-CGC-CPF-CLIENTE-ST TO SLIG22-CLIENTE-ST.
           MOVE  SLIG02-AGENCIA-CLIENTE   TO SLIG22-AGE-OPER.
           MOVE  SLIG02-CCLI-COBR         TO SLIG22-CCLI-COBR.
           MOVE  'SLIG6102'               TO SLIG22-COD-PGM.

           MOVE SLIG02-CGC-CPF-CLIENTE-ST  TO WRK-CPFCGC-ARQERROS.
           MOVE SLIG02-FILIAL-CLIENTE-ST(2:4) TO WRK-FILIAL-ARQERROS.
           MOVE SLIG02-CONTROLE-CLIENTE-ST TO WRK-CTRL-ARQERROS.

           MOVE  WRK-MENSA-ARQERROS       TO SLIG22-DESCR-ERRO.

           WRITE   REG-ERRO.

           MOVE  WRK-GRAVACAO              TO  WRK-OPERACAO.

           PERFORM 114000-TESTAR-FS-ARQERROS.

           ADD 1                           TO  ACU-GRAVA-ARQERROS.

      *----------------------------------------------------------------*
       350000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LIDOS-CLIENDUP      GREATER ZEROS
               PERFORM 310000-GRAVAR-CLIENUNI
           END-IF.

           PERFORM 410000-IMPRIMIR-SRELCTRL.

CNPJAN     DISPLAY 'QTDE CGC NAO POPULADO SLIG97 = '
CNPJAN                                 ACU-CGC-NAO-POP-SLIG97
CNPJAN     DISPLAY 'QTDE CGC NAO POPULADO DEVE17 = '
CNPJAN                                 ACU-CGC-NAO-POP-DEVE17

           CLOSE PERIDECE CADACBCL
                 CLIENUNI ARQERROS SRELCTRL.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       410000-IMPRIMIR-SRELCTRL        SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-CLIENDUP     TO LT1-LIDOS-CLIENDUP.
           MOVE ACU-LIDOS-CADACLIE     TO LT2-LIDOS-CADACLIE.
           MOVE ACU-LIDOS-CADACBCL     TO LT2-LIDOS-CADACBCL.
           MOVE ACU-GRAVA-CLIENUNI     TO LT3-GRAVA-CLIENUNI.
           MOVE ACU-GRAVA-ARQERROS     TO LT4-GRAVA-ARQERROS.
           MOVE ACU-DESPR-CLIENDUP     TO LT5-DESPR-CLIENDUP.
           MOVE WRK-TOT-GRUP-PESQ      TO LT6-GRUP-PESQ.
           MOVE WRK-TOT-GRUP-ACHOU     TO LT7-GRUP-ACHOU.
           MOVE WRK-TOT-GRUP-NAO-ACHOU TO LT8-GRUP-NAO-ACHOU.
           MOVE ACU-ACHOU-CADACLIE     TO LT9-ACHOU-CADACLIE.
           MOVE ACU-NAO-ACHOU-CADACLIE TO LT10-NAO-ACHOU-CADACLIE.
           MOVE ACU-ACHOU-CADACBCL     TO LT11-ACHOU-CADACBCL.
           MOVE ACU-NAO-ACHOU-CADACBCL TO LT12-NAO-ACHOU-CADACBCL.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-SRELCTRL          FROM CABEC1.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC2.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC3.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM CABEC4.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT1.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT2A.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT3.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT4.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT5.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT6.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT7.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT8.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT9.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT10.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT11.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

           WRITE REG-SRELCTRL          FROM LINTOT12.
           PERFORM 112000-TESTAR-FS-SRELCTRL.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       420000-OBTER-GRUPO-CLIENTE      SECTION.
      *----------------------------------------------------------------*

           IF (SLIG02-CGC-CPF-CLIENTE-ST  NOT = WRK-CGC-CPF-CLI-ANT) OR
              (SLIG02-FILIAL-CLIENTE-ST   NOT = WRK-FILIAL-CLI-ANT)  OR
              (SLIG02-CONTROLE-CLIENTE-ST NOT = WRK-CONTROLE-CLI-ANT)

              INITIALIZE                      GRUPW01S-REG-EMPRESA

              MOVE '00'                      TO GRUPW01S-CGC-CPF(1:2)
              MOVE SLIG02-CGC-CPF-CLIENTE-ST TO GRUPW01S-CGC-CPF(3:9)
              IF SLIG02-FILIAL-CLIENTE-ST   EQUAL SPACES
                 MOVE ZEROS                 TO SLIG02-FILIAL-CLIENTE-ST
              END-IF

              MOVE SLIG02-FILIAL-CLIENTE-ST(2:4)
                                             TO GRUPW01S-FILIAL-CGC-CPF
              MOVE SLIG02-CONTROLE-CLIENTE-ST
                                           TO GRUPW01S-CTRL-CGC-CPF

              PERFORM 430000-ACESSAR-MODULO-GRUP0930

              MOVE SLIG02-CGC-CPF-CLIENTE-ST  TO WRK-CGC-CPF-CLI-ANT
              MOVE SLIG02-FILIAL-CLIENTE-ST   TO WRK-FILIAL-CLI-ANT
              MOVE SLIG02-CONTROLE-CLIENTE-ST TO WRK-CONTROLE-CLI-ANT
           END-IF.

CNPJAN     MOVE WRK-NRO-GRUPO-CLIENTE  TO WRK-GRUPO-N
CNPJAN     MOVE WRK-GRUPO-X            TO SLIG02-CGC-GRUPO-CLIENTE-ST.

      *----------------------------------------------------------------*
       420000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       430000-ACESSAR-MODULO-GRUP0930  SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6102'                 TO GRUPW01S-PGM-CHAMADOR.
           MOVE 'GRUP201C'                 TO WRK-MODULO.
           MOVE  SPACES                    TO COMU-IO-PCB
                                              COMU-ALT-PCB.

           CALL WRK-MODULO                 USING GRUPW01S-REG-EMPRESA
                                                 COMU-IO-PCB
                                                 COMU-ALT-PCB.

           ADD  1                          TO WRK-TOT-GRUP-PESQ.

           IF  GRUPW01S-RETURN-CODE        NOT EQUAL ZEROS
               IF GRUPW01S-RETURN-CODE  EQUAL 1
                  MOVE ZEROS               TO GRUPW01S-NUMERO-GRUPO-RET
                  ADD  1                   TO WRK-TOT-GRUP-NAO-ACHOU
               ELSE
                  PERFORM 431000-TRATA-ERRO-GRUP0930
               END-IF
           ELSE
               ADD  1                      TO WRK-TOT-GRUP-ACHOU
           END-IF.

           MOVE GRUPW01S-NUMERO-GRUPO-RET   TO WRK-NRO-GRUPO-CLIENTE.

      *----------------------------------------------------------------*
       430000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       431000-TRATA-ERRO-GRUP0930      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************** SLIG6102 ******************'.
           DISPLAY '*                                            *'.
           DISPLAY '*     ERRO NO ACESSO AO MODULO GRUP0930      *'.
           DISPLAY '*                                            *'.
           DISPLAY '*       ARGUMENTO INCONSISTENTE (004)        *'.
           DISPLAY '*             OU ERRO DB2 (999)              *'.
           DISPLAY '*                                            *'.
           DISPLAY '*------------------------------------------- *'.
           DISPLAY '*  RETURN-CODE........: ' GRUPW01S-RETURN-CODE
                                              '                  *'.
           DISPLAY '*  CNPJ-CPF-CLI.......: ' GRUPW01S-CGC-CPF
                                                      '          *'.
           DISPLAY '*  FILIAL-CLI.........: ' GRUPW01S-FILIAL-CGC-CPF
                                               '                 *'.
           DISPLAY '*  CONTROLE-CLI.......: ' GRUPW01S-CTRL-CGC-CPF
                                             '                   *'.
           DISPLAY '*                                            *'.
           DISPLAY '****************** SLIG6102 ******************'.

           CALL 'ILBOABN0' USING WRK-ABEND.

      *----------------------------------------------------------------*
       431000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'SLIG6102'             TO ERR-PGM

           CALL 'POOL7100'       USING WRK-BATCH  ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

