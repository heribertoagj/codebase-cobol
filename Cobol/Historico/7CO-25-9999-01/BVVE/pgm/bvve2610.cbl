      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     BVVE2610.
       AUTHOR.         MARIA GORETI.


      *----------------------------------------------------------------*
      *            S T E F A N I N I     C O N S U L T O R I A         *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMA     : BVVE2610                                   *
      *      PROGRAMADORA : MARIA GORETI               -  STEFANINI    *
      *      ANALISTA     : LEANDRA    -   DTS         -  GRUPO 90     *
      *      DATA         : 03/11/2003                                 *
      *                                                                *
      *      OBJETIVO     : MANUTENCAO AUTENTICACAO BANCARIA -         *
      *                     - INCLUSAO -                               *
      *                                                                *
      *      TELA         : BVVE01TM                                   *
      *                                                                *
      *      MODULOS CHAMADOS:                                         *
      *      BVVE7005 - MODULO OBTER BANCO                             *
      *      BVVE7007 - MODULO OBTER CEP                               *
      *      BVVE7008 - MODULO OBTER CLIENTE                           *
      *      BVVE7010 - MODULO OBTER RAMO DE ATIVIDADE                 *
      *      BVVE7022 - MODULO OBTER AGENCIA E CONTA                   *
      *      BVVE7920 - MODULO PARA FAZER A CHAMADA DOS MODULOS        *
      *      POOL0110 - CONSISTIR CNPJ/CPF                             *
      *      POOL0431 - CALCULAR DIGITO VEFIFICADOR - MODULO 11        *
      *      POOL0660 - FORMATAR MENSAGEM DE SAIDA                     *
      *      POOL1205 - CONSISTIR DATA VALIDA                          *
      *      POOL5000 - COMUNICACAO DE SENHAS - ON-LINE - COBOL II     *
      *      POOL7100 - TRATAMENTO DE ERROS DE APLICACAO               *
      *                                                                *
      *      CHAMADOR: BVVE2600                                        *
      *                                                                *
      *----------------------------------------------------------------*
      *                      A L T E R A C A O                         *
      *----------------------------------------------------------------*
      *      PROGRAMADOR  : MARCOS A. CASTRO -  DTS         - GRUPO 87 *
      *      OBJETIVO     : ALTERAR O CAMPO DIGITO DA CONTA  (DIGCTA)  *
      *                     DE NUMERICO PARA ALFANUMERICO PARA         *
      *                    ACEITAR A INCLUSAO DE CONTA COM DIGITO 'P'  *
      *----------------------------------------------------------------*
      *                      A L T E R A C A O                         *
      *----------------------------------------------------------------*
      *      ANALISTA     : FERNANDA RODRIGUES  -  BSI TECNOLOGIA      *
      *      OBJETIVO     : CONSISTIR NUMERO DO ENDERECO               *
1214  *                     RETIRAR OS ZEROS INICIAIS DA DT NASCIMENTO *
      *----------------------------------------------------------------*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  FILLER                      PIC  X(32)      VALUE
           'INICIO DA WORKING BVVE2610'.

      *----------------------------------------------------------------*
      *    DEFINICAO DE AREAS E FUNCOES                                *
      *----------------------------------------------------------------*

       77  WRK-GU                      PIC  X(04)      VALUE 'GU'.
       77  WRK-CHNG                    PIC  X(04)      VALUE 'CHNG'.
       77  WRK-ISRT                    PIC  X(04)      VALUE 'ISRT'.
       77  WRK-COD-USER                PIC  9(07)      VALUE ZEROS.
       77  WRK-COD-BANCO               PIC  9(05)      VALUE ZEROS.
       77  WRK-FUNCAO                  PIC  X(04)      VALUE SPACES.
       77  WRK-NOVA-OPCAO              PIC  9(02)      VALUE ZEROS.
       77  WRK-OPCAO-ATUAL             PIC  9(02)      VALUE ZEROS.
       77  WRK-TELA                    PIC  X(08)      VALUE SPACES.
       77  WRK-MODULO                  PIC  X(08)      VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)     VALUE SPACES.
JEM    01  WRK-VAL-INC-FIL             PIC  X(01)      VALUE SPACES.
JEM    01  WRK-RECEBER                 PIC  X(01)      VALUE SPACES.

       01  WRK-COD-DEPTO.
           03  FILLER                  PIC  9(02)      VALUE ZEROS.
           03  WRK-COD-DEPTO-N         PIC  9(04)      VALUE ZEROS.

       01  WRK-MENSAGEM.
           03  FILLER                  PIC  X(04)      VALUE SPACES.
           03  WRK-TRANSACAO           PIC  X(08)      VALUE SPACES.
           03  FILLER                  PIC  X(1988)    VALUE SPACES.

7C2511*----------------------------------------------------------------*
7C2511*    AREA AUXILIAR  - MODULO BRAD2000                            *
7C2511*----------------------------------------------------------------*
7C2511 01 WRK-BRAD2000                 PIC  X(08)  VALUE 'BRAD2000'.
7C2511 01 WRK-AREA-BRAD2000.
7C2511     05  WRK-CAD-CPFCGC.
7C2511         10  WRK-CAD-NUMERO      PIC X(09)           VALUE SPACES.
7C2511         10  WRK-CAD-FILIAL      PIC X(04)           VALUE SPACES.
7C2511         77  WRK-CAD-CONTROLE    PIC 9(02)           VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA AUXILIAR  - MODULO BVVE7920                            *
      *----------------------------------------------------------------*

       COPY I#BVVE21.

       COPY I#BVVE22.

      *----------------------------------------------------------------*
      *    AREA AUXILIAR  - PARA CALCULO DE DIGITO (AGENCIA/CONTA)     *
      *----------------------------------------------------------------*

       01  WRK-CAMPO-DIG               PIC  9(07)          VALUE ZEROS.
       01  WRK-DIGITO-DIG              PIC  X(01)          VALUE SPACES.
       01  WRK-TAMANHO-DIG             PIC  9(02)          VALUE ZEROS.

      *---------------------------------------------------------------*
      *        POOL1205                                               *
      *---------------------------------------------------------------*

       01  WRK-MENSA-POOL1205          PIC X(50)           VALUE SPACES.


       01  WRK-AREA-POOL1205.
           03 WRK-DATA-ENVIADA         PIC 9(008) COMP-3   VALUE ZEROS.
           03 WRK-OPCAO                PIC X(001)          VALUE SPACES.
           03 WRK-DATA-JULIANA         PIC 9(007) COMP-3   VALUE ZEROS.
           03 WRK-DATA-EDITADA         PIC X(010)          VALUE SPACES.
           03 WRK-DATA-GREGORI         PIC 9(008)          VALUE ZEROS.
           03 WRK-DIA-SEMANA           PIC X(013)          VALUE SPACES.
           03 WRK-MES-EXTENSO          PIC X(009)          VALUE SPACES.
           03 WRK-DIA-UT-ANTER         PIC 9(008)          VALUE ZEROS.
           03 WRK-DIA-UT-POSTE         PIC 9(008)          VALUE ZEROS.
           03 WRK-DIA-UT-POSTER REDEFINES WRK-DIA-UT-POSTE.

              05 DIA                   PIC 9(02).
              05 MES                   PIC 9(02).
              05 ANO                   PIC 9(04).

       01  WRK-DATA-AAAAMMDD.
           03  WRK-ANO-AAAAMMDD        PIC  9(04)      VALUE ZEROS.

           03  WRK-MES-AAAAMMDD        PIC  9(02)      VALUE ZEROS.

           03  WRK-DIA-AAAAMMDD        PIC  9(02)      VALUE ZEROS.


       01  WRK-DATA-AAAAMMDD-R  REDEFINES  WRK-DATA-AAAAMMDD PIC 9(08).


      *----------------------------------------------------------------*
      *    AREA AUXILIAR                                               *
      *----------------------------------------------------------------*

       01  AUXILIARES.
           03  WRK-INCONSISTENTE       PIC  X(01)      VALUE SPACES.

           03 WRK-PAGINA               PIC X(03)       VALUE SPACES.
           03 WRK-PAGINA-R             REDEFINES
              WRK-PAGINA               PIC 9(03).

           03  WRK-JUNTA-ENDERECO.
               05  WRK-JUNTA-END      PIC   X(40)      VALUE SPACES.
               05  WRK-JUNTA-NRO      PIC   X(05)      VALUE SPACES.
               05  WRK-JUNTA-COMPL    PIC   X(19)      VALUE SPACES.

1214   01  WRK-ERR-NUM-ENDERECO        PIC  X(001)     VALUE SPACES.

1214   01  WRK-NUMERO-AUX              PIC  X(005)     VALUE SPACES.
1214   01  FILLER          REDEFINES WRK-NUMERO-AUX.
1214       05 WRK-NUMERO-AUX-N         PIC  9(005).

JEM   *----------------------------------------------------------------*
JEM   *    AREA PARA ERRO NO ACESSO A POOL0110                         *
JEM   *----------------------------------------------------------------*
JEM
JEM    01  WRK-CGC-FILIAL.
7C2511*    03  WRK-CPF-CGC                 PIC 9(009) VALUE ZEROS.
7C2511     03  WRK-CPF-CGC                 PIC X(009) VALUE SPACES.
7C2511*    03  WRK-FILIAL                  PIC 9(005) VALUE ZEROS.
7C2511     03  WRK-FILIAL                  PIC X(004) VALUE SPACES.
7C2511     03  FILLER                      PIC X(01) VALUE SPACES.
JE
JEM    01  WRK-CONTROLE                    PIC X(002) VALUE SPACES.
JEM
JEM   *----------------------------------------------------------------*
JEM   *    AREA PARA ERRO NO ACESSO A POOL0110                         *
JEM   *----------------------------------------------------------------*
JEM
JEM    01  WRK-CGC-FILIAL-P.
7C2511*    03  WRK-CPF-CGC-P               PIC 9(009) VALUE ZEROS.
7C2511     03  WRK-CPF-CGC-P               PIC X(009) VALUE SPACES.
7C2511*    03  WRK-FILIAL-P                PIC 9(005) VALUE ZEROS.
7C2511     03  WRK-FILIAL-P                PIC X(004) VALUE SPACES.
7C2511     03  FILLER                      PIC X(01) VALUE SPACES.
JEM
JEM    01  WRK-CONTROLE-P                  PIC 9(002) VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA UTILIZADA PARA DATA DB2                                *
      *----------------------------------------------------------------*

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(02)      VALUE ZEROS.

           03  FILLER                  PIC  X(01)      VALUE '.'.
           03  WRK-MES-DB2             PIC  9(02)      VALUE ZEROS.

           03  FILLER                  PIC  X(01)      VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(04)      VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA UTILIZADA PELO MODULO  -  BVVE7001                     *
      *----------------------------------------------------------------*

       01  WRK-ENVIA-DADOS-PROPOSTA-I.
      *--------- PROPOSTA ---------------------------------------------*
           03  WRK-E-CNPJCPF.
7C2511*        05  WRK-E-CCNPJ-CPF-ESTBL   PIC 9(009)      VALUE ZEROS.
7C2511         05  WRK-E-CCNPJ-CPF-ESTBL   PIC X(009)      VALUE SPACES.
7C2511*        05  WRK-E-CFLIAL-CNPJ-ESTBL PIC 9(004)      VALUE ZEROS.
7C2511         05  WRK-E-CFLIAL-CNPJ-ESTBL PIC X(004)      VALUE SPACES.
               05  WRK-E-CCTRL-CNPJ-CPF    PIC 9(002)      VALUE ZEROS.
           03  WRK-E-CNPJCPF-R REDEFINES WRK-E-CNPJCPF.
               05  WRK-E-CPF-NUMERO        PIC 9(09).
               05  WRK-E-CPF-CONTR         PIC 9(02).
               05  WRK-E-CPF-FILIAL        PIC X(04).
           03  WRK-E-IESTBL-AFLIA          PIC X(032)      VALUE SPACES.
           03  WRK-E-CINSCR-EST-ESTBL      PIC X(015)      VALUE SPACES.
           03  WRK-E-IFANTS-ESTBL-AFLIA    PIC X(032)      VALUE SPACES.
           03  WRK-E-IPLAQT-EQPMT-ESTBL    PIC X(022)      VALUE SPACES.
           03  WRK-E-CSIT-PPSTA-AFLIA      PIC 9(002)      VALUE ZEROS.
           03  WRK-E-RSIT-PPSTA-AFLIA      PIC X(020)      VALUE SPACES.
           03  WRK-E-DENVIO-PPSTA-AFLIA    PIC X(010)      VALUE SPACES.
           03  WRK-E-DRETOR-PPSTA-AFLIA    PIC X(010)      VALUE SPACES.
           03  WRK-E-DCAD-PPSTA-AFLIA      PIC X(010)      VALUE SPACES.
           03  WRK-E-CTPO-ATVDD-COML       PIC 9(005)      VALUE ZEROS.
           03  WRK-E-RATVDD-COML           PIC X(040)      VALUE SPACES.
           03  WRK-E-CEMPR-CONTT           PIC 9(005)      VALUE ZEROS.
           03  WRK-E-DATULZ-PPSTA-AFLIA    PIC X(026)      VALUE SPACES.
           03  WRK-E-CFUNC-BDSCO           PIC 9(009)      VALUE ZEROS.
           03  WRK-E-CEMPR-INC             PIC 9(005)      VALUE ZEROS.
           03  WRK-E-CJUNC-DEPDC-BDSCO     PIC 9(005)      VALUE ZEROS.

      *---------- PRIMEIRO  ENDERECO ----------------------------------*
           03  WRK-E-CCEP-1                PIC 9(005)      VALUE ZEROS.
           03  WRK-E-CCEP-COMPL-1          PIC 9(003)      VALUE ZEROS.
           03  WRK-E-IMUN-ESTBL-1          PIC X(028)      VALUE SPACES.
           03  WRK-E-EESTBL-AFLIA-1        PIC X(064)      VALUE SPACES.
           03  WRK-E-CSGL-UF-1             PIC X(002)      VALUE SPACES.
           03  WRK-E-EEMAIL-CNTAT-ESTBL-1  PIC X(050)      VALUE SPACES.
           03  WRK-E-ICNTAT-ESTBL-AFLIA-1  PIC X(032)      VALUE SPACES.

      *---------- SEGUNDO ENDERECO ------------------------------------*
           03  WRK-E-CCEP-2                PIC 9(005)      VALUE ZEROS.
           03  WRK-E-CCEP-COMPL-2          PIC 9(003)      VALUE ZEROS.
           03  WRK-E-IMUN-ESTBL-2          PIC X(028)      VALUE SPACES.
           03  WRK-E-EESTBL-AFLIA-2        PIC X(064)      VALUE SPACES.
           03  WRK-E-CSGL-UF-2             PIC X(002)      VALUE SPACES.

      *--------- PRIMEIRO   TELEFONE  ---------------------------------*
           03  WRK-E-CDDD-FONE-ESTBL-1     PIC X(004)      VALUE SPACES.
           03  WRK-E-CFONE-PPRIE-1         PIC 9(011)      VALUE ZEROS.

      *--------- SEGUNDO  TELEFONE  -----------------------------------*
           03  WRK-E-CDDD-FONE-ESTBL-2     PIC X(004)      VALUE SPACES.
           03  WRK-E-CFONE-PPRIE-2         PIC 9(011)      VALUE ZEROS.

      *--------- PRIMEIRO  PROPRIETARIO -------------------------------*
           03  WRK-E-IPPRIE-ESTBL-AFLIA-1  PIC X(032)      VALUE SPACES.
           03  WRK-E-CCPF-PPRIE-1          PIC 9(009)      VALUE ZEROS.
           03  WRK-E-CCTRL-CPD-PPRIE-1     PIC 9(002)      VALUE ZEROS.
           03  WRK-E-DNASC-PPRIE-1         PIC X(010)      VALUE SPACES.

      *---------- SEGUNDO  PROPRIETARIO -------------------------------*
           03  WRK-E-IPPRIE-ESTBL-AFLIA-2  PIC X(032)      VALUE SPACES.
           03  WRK-E-CCPF-PPRIE-2          PIC 9(009)      VALUE ZEROS.
           03  WRK-E-CCPF-REDEF-2          REDEFINES
               WRK-E-CCPF-PPRIE-2          PIC X(009).
           03  WRK-E-CCTRL-CPD-PPRIE-2     PIC 9(002)      VALUE ZEROS.
           03  WRK-E-CCTRL-RED-2           REDEFINES
               WRK-E-CCTRL-CPD-PPRIE-2     PIC X(002).
           03  WRK-E-DNASC-PPRIE-2         PIC X(010)      VALUE SPACES.

      *---------- TERCEIRO PROPRIETARIO -------------------------------*
           03  WRK-E-IPPRIE-ESTBL-AFLIA-3  PIC X(032)      VALUE SPACES.
           03  WRK-E-CCPF-PPRIE-3          PIC 9(009)      VALUE ZEROS.
           03  WRK-E-CCPF-REDEF-3          REDEFINES
               WRK-E-CCPF-PPRIE-3          PIC X(009).
           03  WRK-E-CCTRL-CPD-PPRIE-3     PIC 9(002)      VALUE ZEROS.
           03  WRK-E-CCTRL-RED-3           REDEFINES
               WRK-E-CCTRL-CPD-PPRIE-3     PIC X(002).
           03  WRK-E-DNASC-PPRIE-3         PIC X(010)      VALUE SPACES.

      *--------- DEMAIS CAMPOS PROPOSTA -------------------------------*
           03  WRK-E-CBANCO                PIC 9(003)      VALUE ZEROS.
           03  WRK-E-CAG-BCRIA             PIC 9(005)      VALUE ZEROS.
           03  WRK-E-CCTA-BCRIA-CLI        PIC 9(013)      VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA UTILIZADA PELO MODULO  -  BVVE7010                     *
      *----------------------------------------------------------------*

       01  WRK-ENVIA-DADOS-RAMO.
           03  WRK-E-COD-RAMO              PIC 9(05)       VALUE ZEROS.

       01  WRK-RECEBE-DADOS-RAMO.
           03  WRK-S-COD-RAMO              PIC 9(05)       VALUE ZEROS.
           03  WRK-S-DESC-RAMO             PIC X(40)       VALUE SPACES.
           03  FILLER                      PIC X(1999)     VALUE SPACES.

      *----------------------------------------------------------------*
      *    AREA UTILIZADA PELO MODULO  -  BVVE7007                     *
      *----------------------------------------------------------------*

       01  WRK-ENVIA-DADOS-CEP.
           03  WRK-E-CEP                   PIC 9(05)       VALUE ZEROS.
           03  WRK-E-COMPL-CEP             PIC 9(03)       VALUE ZEROS.

       01  WRK-RECEBE-DADOS-CEP.
           03  WRK-S-ILOGDR-CEP            PIC X(60)       VALUE SPACES.
           03  WRK-S-EBAIRRO-LOGDR         PIC X(20)       VALUE SPACES.
           03  WRK-S-IMUN                  PIC X(30)       VALUE SPACES.
           03  WRK-S-CSGL-UF               PIC X(02)       VALUE SPACES.
           03  WRK-S-CCEP                  PIC 9(05)       VALUE ZEROS.
           03  WRK-S-CCEP-COMPL            PIC 9(03)       VALUE ZEROS.
           03  FILLER                      PIC X(1924)     VALUE SPACES.

      *----------------------------------------------------------------*
      *    AREA UTILIZADA PELO MODULO  -  BVVE7008                     *
      *----------------------------------------------------------------*

       01  WRK-ENVIA-DADOS-CLIENTE.
           03  WRK-E-TIPO-CLIE             PIC 9(01)       VALUE ZEROS.
7C2511*    03  WRK-E-NUMERO-CLIE           PIC 9(09)       VALUE ZEROS.
7C2511     03  WRK-E-NUMERO-CLIE           PIC X(09)       VALUE SPACES.
7C2511*    03  WRK-E-FILIAL-CLIE           PIC 9(04)       VALUE ZEROS.
7C2511     03  WRK-E-FILIAL-CLIE           PIC X(04)       VALUE SPACES.
           03  WRK-E-CONTROLE-CLIE         PIC 9(02)       VALUE ZEROS.
           03  WRK-E-BANCO-CLIE            PIC 9(03)       VALUE ZEROS.
           03  WRK-E-AGENCIA-CLIE          PIC 9(05)       VALUE ZEROS.
           03  WRK-E-CONTA-CLIE            PIC 9(13)       VALUE ZEROS.
MARC       03  WRK-E-DIG-CONTA-CLIE        PIC X(01)       VALUE SPACES.
MARC  *    03  WRK-E-DIG-CONTA-CLIE        PIC 9(01).

       01  WRK-RECEBE-DADOS-CLIENTE.
7C2511*    03  WRK-S-NUMERO-CLIE           PIC 9(09)       VALUE ZEROS.
7C2511     03  WRK-S-NUMERO-CLIE           PIC X(09)       VALUE SPACES.
7C2511*    03  WRK-S-FILIAL-CLIE           PIC 9(04)       VALUE ZEROS.
7C2511     03  WRK-S-FILIAL-CLIE           PIC X(04)       VALUE SPACES.
           03  WRK-S-CONTROLE-CLIE         PIC 9(02)       VALUE ZEROS.
           03  WRK-S-NOME-CLIE             PIC X(40)       VALUE SPACES.
           03  WRK-S-ENDER-CLIE            PIC X(60)       VALUE SPACES.
           03  WRK-S-BAIRRO-CLIE           PIC X(20)       VALUE SPACES.
           03  WRK-S-NR-ENDER-CLIE-A.
               05  FILLER                  PIC X(02)       VALUE SPACES.
               05  WRK-S-NR-ENDER-CLIE     PIC X(05)       VALUE SPACES.
           03  WRK-S-COMPL-CLIE            PIC X(20)       VALUE SPACES.
           03  WRK-S-CIDADE-CLIE           PIC X(30)       VALUE SPACES.
           03  WRK-S-UF-CLIE               PIC X(02)       VALUE SPACES.
           03  WRK-S-CEP-CLIE              PIC 9(05)       VALUE ZEROS.
           03  WRK-S-CEP-COMPL-CLIE        PIC 9(03)       VALUE ZEROS.
           03  WRK-S-DDD-FONE-CLIE         PIC X(04)       VALUE SPACES.
           03  WRK-S-FONE-CLIE             PIC 9(11)       VALUE ZEROS.
           03  WRK-S-PROPR1-CLIE           PIC X(32)       VALUE SPACES.
7C2511*    03  WRK-S-CPF1-CLIE             PIC 9(09)       VALUE ZEROS.
7C2511     03  WRK-S-CPF1-CLIE             PIC X(09)       VALUE SPACES.
           03  WRK-S-CTRLCPF1-CLIE         PIC 9(02)       VALUE ZEROS.
           03  WRK-S-PROPR2-CLIE           PIC X(32)       VALUE SPACES.
7C2511*    03  WRK-S-CPF2-CLIE             PIC 9(09)       VALUE ZEROS.
7C2511     03  WRK-S-CPF2-CLIE             PIC X(09)       VALUE SPACES.
           03  WRK-S-CTRLCPF2-CLIE         PIC 9(02)       VALUE ZEROS.
           03  WRK-S-PROPR3-CLIE           PIC X(32)       VALUE SPACES.
7C2511*    03  WRK-S-CPF3-CLIE             PIC 9(09)       VALUE ZEROS.
7C2511     03  WRK-S-CPF3-CLIE             PIC X(09)       VALUE SPACES.
           03  WRK-S-CTRLCPF3-CLIE         PIC 9(02)       VALUE ZEROS.
           03  WRK-S-CBCO                  PIC 9(03)       VALUE ZEROS.
           03  WRK-S-AG-BCARIA             PIC 9(05)       VALUE ZEROS.
           03  WRK-S-CCTA-BCARIA           PIC 9(13)       VALUE ZEROS.
           03  WRK-S-DIG-CCTA-BCARIA       PIC X(01)       VALUE SPACES.
MARC  *    03  WRK-S-DIG-CCTA-BCARIA       PIC 9(01).
           03  FILLER                      PIC X(1668)     VALUE SPACES.

      *----------------------------------------------------------------*
      *    AREA UTILIZADA PELO MODULO  -  BVVE7005                     *
      *----------------------------------------------------------------*

       01  WRK-ENVIA-DADOS-BANCO.
           03  WRK-E-BANCO                 PIC 9(03)       VALUE ZEROS.

       01  WRK-RECEBE-DADOS-BANCO.
           03  WRK-S-BANCO                 PIC 9(03)       VALUE ZEROS.
           03  WRK-S-NM-BANCO              PIC X(40)       VALUE SPACES.
           03  FILLER                      PIC X(2001)     VALUE SPACES.

      *----------------------------------------------------------------*
      *    AREA UTILIZADA PELO MODULO  -  BVVE7006                     *
      *----------------------------------------------------------------*

       01  WRK-ENVIA-DADOS-AGENC.
           03  WRK-E-BCO-AGENCIA           PIC 9(03)       VALUE ZEROS.
           03  WRK-E-AGE-AGENCIA           PIC 9(05)       VALUE ZEROS.

       01  WRK-RECEBE-DADOS-AGENC.
           03  WRK-S-BCO-AGENCIA           PIC 9(03)       VALUE ZEROS.
           03  WRK-S-AGE-AGENCIA           PIC 9(05)       VALUE ZEROS.
           03  WRK-S-NM-AGENCIA            PIC X(40)       VALUE SPACES.
           03  FILLER                      PIC X(1996)     VALUE SPACES.

      *----------------------------------------------------------------*
      *    ATRIBUTOS                                                   *
      *----------------------------------------------------------------*

      *-DEFINICAO DOS ATRIBUTOS ---------------------------------------*

       01  FILLER.
           03  TAB-ATRIBUTOS.
      *----------------------------------------------------------------*
      *    BRILHANTE, DESPROT, COM POS. CURSOR, NUM        -  1        *
      *----------------------------------------------------------------*
               05  WRK-49369           PIC S9(008) COMP VALUE +49369.
      *----------------------------------------------------------------*
      *    NORMAL, DESPROT, COM POS. CURSOR, NUM           -  2        *
      *----------------------------------------------------------------*
               05  WRK-49361           PIC S9(008) COMP VALUE +49361.
      *----------------------------------------------------------------*
      *    PROTEGIDO (NUMERICO OU ALFA)                    - 3         *
      *----------------------------------------------------------------*
               05  WRK-225             PIC S9(008) COMP VALUE +225.
      *----------------------------------------------------------------*
      *    BRILHANTE, DESPROT, COM POS. CURSOR, ALFA       - 4         *
      *----------------------------------------------------------------*
               05  WRK-49353           PIC S9(008) COMP VALUE +49353.
      *----------------------------------------------------------------*
      *    NORMAL, DESPROT, CURSOR, ALFA                   -  5        *
      *----------------------------------------------------------------*
               05  WRK-49345           PIC S9(008) COMP VALUE +49345.
      *----------------------------------------------------------------*
      *    NORMAL, DESPROT, ALFA                           -  6        *
      *----------------------------------------------------------------*
               05  WRK-193             PIC S9(008) COMP VALUE +193.
      *----------------------------------------------------------------*
      *    PROTEGIDO, NUMERICO, COM POS. CURSOR            -  7        *
      *----------------------------------------------------------------*
               05  WRK-241             PIC S9(008) COMP VALUE +241.

      *    03  FILLER   REDEFINES   TAB-ATRIBUTOS   OCCURS   07  TIMES.

      *        05  TAB-ATRIBUTO        PIC S9(008) COMP.

      *----------------------------------------------------------------*
      *    AREA PARA ERRO NO ACESSO A POOL0660                         *
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL0660.
           03  FILLER                  PIC  X(47)      VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA POOL0660 *'.
           03  FILLER                  PIC  X(12)      VALUE
               '* RET.COD = '.
           03  WRK-RETURN-CODE         PIC  9(02)BB    VALUE ZEROS.
           03  FILLER                  PIC  X(08)      VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC  9(02)      VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO                                         *
      *----------------------------------------------------------------*
       COPY I#BVVE25.

      *----------------------------------------------------------------*
      *    INCLUSAO DA POOL7100 - TRATA ERRO                           *
      *----------------------------------------------------------------*
       COPY POL7100C.

      *----------------------------------------------------------------*
      *    MENSAGENS UTILIZADAS PELO PROGRAMA                          *
      *----------------------------------------------------------------*

       01  FILLER.

           03  WRK-MSG01               PIC  X(79)      VALUE
               'PF INVALIDA'.

           03  WRK-MSG02               PIC  X(79)      VALUE
              'RAMO DE ATIVIDADE NAO ENCONTRADO'.

           03  WRK-MSG03               PIC  X(79)      VALUE
               'CNPJ/CPF NAO ENCONTRADO NO CADASTRO DE CLIENTES'.

           03  WRK-MSG04               PIC  X(79)      VALUE
               'DIGITE O(S) DADO(S) E TECLE <ENTER>'.

           03  WRK-MSG05               PIC  X(79)      VALUE
               'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'.

           03  WRK-MSG06               PIC  X(79)      VALUE
               'PROPOSTA JA EXISTE    '.

           03  WRK-MSG07               PIC  X(79)      VALUE
               'INCLUSAO EFETUADA COM SUCESSO  '.

           03  WRK-MSG08               PIC  X(79)      VALUE
               'DADO(S) TECLADO(S) INCONSISTENTE(S)  '.

           03  WRK-MSG09               PIC  X(79)      VALUE
               'PREENCHIMENTO OBRIGATORIO PARA ESTA OPCAO'.

           03  WRK-MSG10.
               05  FILLER              PIC  X(17)      VALUE
                   'ERRO NO ACESSO A '.
               05  WRK-MSG10-NM-POOL   PIC  X(08)      VALUE SPACES.
               05  FILLER              PIC  X(08)      VALUE
                   ' - RC = '.
               05  WRK-MSG10-RET-CODE  PIC  9(04)      VALUE ZEROS.

           03  WRK-MSG11               PIC  X(79)      VALUE
               'RAMO DE ATIVIDADE INEXISTENTE '.

           03  WRK-MSG12               PIC  X(79)      VALUE
               'CEP INEXISTENTE '.

           03  WRK-MSG13               PIC  X(79)      VALUE
               'DATA NASCIMENTO INVALIDA '.

           03  WRK-MSG14               PIC  X(79)      VALUE
               'CONTROLE DO CPF INVALIDO '.

           03  WRK-MSG15               PIC  X(79)      VALUE
               'CODIGO DO BANCO INVALIDO '.

           03  WRK-MSG16               PIC  X(79)      VALUE
               'AGENCIA INVALIDA '.

           03  WRK-MSG17               PIC  X(79)      VALUE
               'AGENCIA/CONTA INFORMADA NAO CONFERE COM CNPJ/CPF '.

           03  WRK-MSG18               PIC  X(79)      VALUE
               'DIGITO DA CONTA INVALIDO'.

           03  WRK-MSG19               PIC  X(79)      VALUE
               'PROPRIETARIOS COM MESMO CPF'.
           03  WRK-MSG20               PIC  X(79)      VALUE
               'CPF INVALIDO'.
           03  WRK-MSG21               PIC  X(79)      VALUE
               'NUMERO DO ENDERECO DEVE SER MAIOR QUE ZEROS OU "S/N"'.
JEM        03  WRK-MSG41               PIC  X(79)      VALUE
JEM            'INCLUSAO EFETUADA COM SUCESSO. INFORME OS DADOS PARA INC
JEM   -        'LUSAO DA FILIAL'.
JEM        03  WRK-MSG42               PIC  X(79)      VALUE     'TECLE
JEM   -        '<PF6> PARA CONFIRMAR OU <PF11> PARA INCLUIR FILIAL'.
JEM        03  WRK-MSG43               PIC  X(79)      VALUE
JEM            'CNPJ INVALIDO'.
JEM   *    03  WRK-MSG44               PIC  X(79)      VALUE
JEM   *        'CONTA POUPANCA N O PERMITIDA'.


      *----------------------------------------------------------------*
      *    TELA BVVE01TM            -  INPUT                           *
      *----------------------------------------------------------------*

       01  INPUT-01TM.
           03  INP-LL-01TM                 PIC 9(04) COMP  VALUE ZEROS.
           03  INP-ZZ-01TM                 PIC 9(04) COMP  VALUE ZEROS.
           03  INP-TRANCODE-01TM           PIC X(08)       VALUE SPACES.
           03  FILLER                      PIC X(06)       VALUE SPACES.
           03  INP-PFK-01TM                PIC X(01)       VALUE SPACES.
           03  INP-SENHAS-01TM             PIC X(37)       VALUE SPACES.
           03  INP-NOPCAO-01TM             PIC X(02)       VALUE SPACES.
           03  INP-FASE-01TM               PIC X(01)       VALUE SPACES.
           03  INP-CHAMADOR-01TM           PIC X(08)       VALUE SPACES.
           03  INP-PAG2660-01TM.
               05  INP-PAG2660-01TM-N      PIC 9(03)       VALUE ZEROS.
           03  WRK-INP-ATRIBS-01TM         PIC X(48)       VALUE SPACES.
           03  INP-CNPJNRO-01TM.
7C2511*        05  INP-CNPJNRO-01TM-N      PIC  9(09)      VALUE ZEROS.
7C2511         05  INP-CNPJNRO-01TM-X      PIC  X(09)      VALUE SPACES.
           03  INP-CNPJFIL-01TM.
7C2511*        05  INP-CNPJFIL-01TM-N      PIC  9(05)      VALUE ZEROS.
7C2511         05  INP-CNPJFIL-01TM-X      PIC  X(04)      VALUE SPACES.
7C2511         05  FILLER                  PIC  X(01)      VALUE SPACES.
           03  INP-CNPJCTR-01TM.
               05  INP-CNPJCTR-01TM-N      PIC  9(02)      VALUE ZEROS.
           03  INP-NOMEPRO-01TM            PIC  X(32)      VALUE SPACES.
EMER       03  INP-INSCRES-01TM.
EMER           05  INP-INSCRES-01TM-N      PIC  9(15)      VALUE ZEROS.
           03  INP-RAMO-01TM.
               05  INP-RAMO-01TM-N         PIC  9(05)      VALUE ZEROS.
           03  INP-IRAMO-01TM              PIC  X(40)      VALUE SPACES.
           03  INP-NMFANT-01TM             PIC  X(32)      VALUE SPACES.
           03  INP-NMPLAQ-01TM             PIC  X(22)      VALUE SPACES.

           03  INP-CEPNRO1-01TM.
               05  INP-CEPNRO1-01TM-N      PIC  9(05)      VALUE ZEROS.
           03  INP-CEPSUF1-01TM.
               05  INP-CEPSUF1-01TM-N      PIC  9(03)      VALUE ZEROS.
           03  INP-ENDER1-01TM             PIC  X(40)      VALUE SPACES.
           03  INP-NUMERO1-01TM.
               05  INP-NUMERO1-01TM-N      PIC  9(05)      VALUE ZEROS.
           03  INP-COMPL1-01TM             PIC  X(19)      VALUE SPACES.
           03  INP-CIDADE1-01TM            PIC  X(28)      VALUE SPACES.
           03  INP-UF1-01TM                PIC  X(02)      VALUE SPACES.

           03  INP-CEPNRO2-01TM.
               05  INP-CEPNRO2-01TM-N      PIC  9(05)      VALUE ZEROS.
           03  INP-CEPSUF2-01TM.
               05  INP-CEPSUF2-01TM-N      PIC  9(03)      VALUE ZEROS.
           03  INP-CONTAT1-01TM            PIC  X(32)      VALUE SPACES.
           03  INP-ENDER2-01TM             PIC  X(40)      VALUE SPACES.
           03  INP-NUMERO2-01TM.
               05  INP-NUMERO2-01TM-N      PIC  9(05)      VALUE ZEROS.
           03  INP-COMPL2-01TM             PIC  X(19)      VALUE SPACES.
           03  INP-CIDADE2-01TM            PIC  X(28)      VALUE SPACES.
           03  INP-UF2-01TM                PIC  X(02)      VALUE SPACES.
           03  INP-TDDD1-01TM              PIC  X(04)      VALUE SPACES.
           03  INP-TFONE1-01TM.
               05  INP-TFONE1-01TM-N       PIC  9(09)      VALUE ZEROS.
           03  INP-FDDD1-01TM              PIC  X(04)      VALUE SPACES.
           03  INP-FFONE1-01TM.
               05  INP-FFONE1-01TM-N       PIC  9(09)      VALUE ZEROS.
           03  INP-CONTR1-01TM.
               05  INP-CONTR1-01TM-N       PIC  9(05)      VALUE ZEROS.
           03  INP-EMAIL1-01TM             PIC  X(50)      VALUE SPACES.

           03  INP-CEPNRO3-01TM.
               05  INP-CEPNRO3-01TM-N      PIC  9(05)      VALUE ZEROS.
           03  INP-CEPSUF3-01TM.
               05  INP-CEPSUF3-01TM-N      PIC  9(03)      VALUE ZEROS.
           03  INP-ENDER3-01TM             PIC  X(40)      VALUE SPACES.
           03  INP-NUMERO3-01TM.
               05  INP-NUMERO3-01TM-N      PIC  9(05)      VALUE ZEROS.
           03  INP-COMPL3-01TM             PIC  X(19)      VALUE SPACES.
           03  INP-CIDADE3-01TM            PIC  X(28)      VALUE SPACES.
           03  INP-UF3-01TM                PIC  X(02)      VALUE SPACES.

           03  INP-PROPR1-01TM             PIC  X(32)      VALUE SPACES.
           03  INP-CPFP1-01TM.
               05  INP-CPFP1-01TM-N        PIC  9(09)      VALUE ZEROS.
           03  INP-DIGP1-01TM.
               05  INP-DIGP1-01TM-N        PIC  9(02)      VALUE ZEROS.
1214       03  INP-DDNASC1-01TM.
1214           05  WRK-INP-DDNASC1-01TM-X  PIC  X(02)      VALUE SPACES.
1214           05  INP-DDNASC1-01TM-N  REDEFINES WRK-INP-DDNASC1-01TM-X
1214                                       PIC  9(02).
1214       03  INP-MMNASC1-01TM.
1214           05  WRK-INP-MMNASC1-01TM-X  PIC  X(02)      VALUE SPACES.
1214           05  INP-MMNASC1-01TM-N  REDEFINES WRK-INP-MMNASC1-01TM-X
1214                                       PIC  9(02).
1214       03  INP-AANASC1-01TM.
1214           05  WRK-INP-AANASC1-01TM-X  PIC  X(04)      VALUE SPACES.
1214           05  INP-AANASC1-01TM-N  REDEFINES WRK-INP-AANASC1-01TM-X
1214                                       PIC  9(04).

           03  INP-PROPR2-01TM             PIC  X(32)      VALUE SPACES.
           03  INP-CPFP2-01TM.
               05  INP-CPFP2-01TM-N        PIC  9(09)      VALUE ZEROS.
           03  INP-DIGP2-01TM.
               05  INP-DIGP2-01TM-N        PIC  9(02)      VALUE ZEROS.
1214       03  INP-DDNASC2-01TM.
1214           05  WRK-INP-DDNASC2-01TM-X  PIC  X(02)      VALUE SPACES.
1214           05  INP-DDNASC2-01TM-N  REDEFINES WRK-INP-DDNASC2-01TM-X
1214                                       PIC  9(02).
1214       03  INP-MMNASC2-01TM.
1214           05  WRK-INP-MMNASC2-01TM-X  PIC  X(02)      VALUE SPACES.
1214           05  INP-MMNASC2-01TM-N  REDEFINES WRK-INP-MMNASC2-01TM-X
1214                                       PIC  9(02).
1214       03  INP-AANASC2-01TM.
1214           05  WRK-INP-AANASC2-01TM-X  PIC  X(04)      VALUE SPACES.
1214           05  INP-AANASC2-01TM-N  REDEFINES WRK-INP-AANASC2-01TM-X
1214                                       PIC  9(04).

           03  INP-PROPR3-01TM             PIC  X(32)      VALUE SPACES.
           03  INP-CPFP3-01TM.
               05  INP-CPFP3-01TM-N        PIC  9(09)      VALUE ZEROS.
           03  INP-DIGP3-01TM.
               05  INP-DIGP3-01TM-N        PIC  9(02)      VALUE ZEROS.
1214       03  INP-DDNASC3-01TM.
1214           05  WRK-INP-DDNASC3-01TM-X  PIC  X(02)      VALUE SPACES.
1214           05  INP-DDNASC3-01TM-N  REDEFINES WRK-INP-DDNASC3-01TM-X
1214                                       PIC  9(02).
1214       03  INP-MMNASC3-01TM.
1214           05  WRK-INP-MMNASC3-01TM-X  PIC  X(02)      VALUE SPACES.
1214           05  INP-MMNASC3-01TM-N  REDEFINES WRK-INP-MMNASC3-01TM-X
1214                                       PIC  9(02).
1214       03  INP-AANASC3-01TM.
1214           05  WRK-INP-AANASC3-01TM-X  PIC  X(04)      VALUE SPACES.
1214           05  INP-AANASC3-01TM-N  REDEFINES WRK-INP-AANASC3-01TM-X
1214                                       PIC  9(04).

           03  INP-BANCO-01TM.
               05  INP-BANCO-01TM-N        PIC  9(03)      VALUE ZEROS.
           03  INP-AGENCIA-01TM.
               05  INP-AGENCIA-01TM-N      PIC  9(05)      VALUE ZEROS.
           03  INP-CONTA-01TM.
               05  INP-CONTA-01TM-N        PIC  9(07)      VALUE ZEROS.
           03  INP-DIGCTA-01TM.
MARC  *        05  INP-DIGCTA-01TM-N       PIC  9(01).
MARC           05  INP-DIGCTA-01TM-N       PIC  X(01)      VALUE SPACES.
JEM        03  WRK-INP-INCALT-01TM         PIC  X(01)      VALUE SPACES.
           03  INP-MENSAG-01TM             PIC  X(79)      VALUE SPACES.

      *----------------------------------------------------------------*
      *    TELA BVVE01TM            -  OUTPUT                          *
      *----------------------------------------------------------------*

       01  OUTPUT-01TM.
JEM        03  WRK-OUT-LL-01TM             PIC 9(04) COMP  VALUE 1045.
           03  OUT-ZZ-01TM                 PIC 9(04) COMP  VALUE ZEROS.
           03  OUT-DADOS-01TM.
               05  OUT-SENHAS-01TM         PIC X(37)       VALUE SPACES.
               05  OUT-NOPCAO-01TM         PIC X(02)       VALUE SPACES.
               05  OUT-FASE-01TM           PIC X(01)       VALUE SPACES.
               05  OUT-CHAMADOR-01TM       PIC X(08)       VALUE SPACES.
               05  OUT-PAG2660-01TM        PIC X(03)       VALUE SPACES.
               05  WRK-OUT-ATRIBS-01TM     PIC X(48)       VALUE SPACES.
7C2511*        05  OUT-CNPJNRO-01TM.
7C2511*            07  OUT-CNPJNRO-01TM-N  PIC 9(09)       VALUE ZEROS.
7C2511         05  OUT-CNPJNRO-01TM.
7C2511             07  OUT-CNPJNRO-01TM-X  PIC X(09)       VALUE SPACES.
7C2511             07  OUT-CNPJNRO-01TM-N  REDEFINES OUT-CNPJNRO-01TM-X
7C2511                                           PIC 9(09).
               05  WRK-OUT-ATTR-CNPJFIL-01TM PIC 9(04) COMP VALUE ZEROS.
7C2511*        05  OUT-CNPJFIL-01TM.
7C2511*            07  OUT-CNPJFIL-01TM-N  PIC 9(05)       VALUE ZEROS.
7C2511         05  OUT-CNPJFIL-01TM.
7C2511             07  OUT-CNPJFIL-01TM-X  PIC X(04)       VALUE SPACES.
7C2511             07  OUT-CNPJFIL-01TM-N  REDEFINES OUT-CNPJFIL-01TM-X
7C2511                                     PIC 9(04).
7C2511         05  FILLER                  PIC X(01)       VALUE SPACES.
               05  WRK-OUT-ATTR-CNPJCTR-01TM PIC 9(04) COMP VALUE ZEROS.
               05  OUT-CNPJCTR-01TM.
                   07  OUT-CNPJCTR-01TM-N  PIC 9(02)       VALUE ZEROS.
               05  OUT-NOMEPRO-01TM        PIC X(32)       VALUE SPACES.
               05  OUT-ATTR-INSCRES-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-INSCRES-01TM        PIC X(15)       VALUE SPACES.
               05  OUT-ATTR-RAMO-01TM      PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-RAMO-01TM.
                   07  OUT-RAMO-01TM-N     PIC 9(05)       VALUE ZEROS.
               05  OUT-IRAMO-01TM          PIC X(40)       VALUE SPACES.
               05  OUT-ATTR-NMFANT-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-NMFANT-01TM         PIC X(32)       VALUE SPACES.
               05  OUT-ATTR-NMPLAQ-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-NMPLAQ-01TM         PIC X(22)       VALUE SPACES.

               05  OUT-ATTR-CEPNRO1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CEPNRO1-01TM.
                   07  OUT-CEPNRO1-01TM-N  PIC 9(05)       VALUE ZEROS.
               05  OUT-ATTR-CEPSUF1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CEPSUF1-01TM.
                   07  OUT-CEPSUF1-01TM-N  PIC 9(03)       VALUE ZEROS.
               05  OUT-ATTR-ENDER1-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-ENDER1-01TM         PIC X(40)       VALUE SPACES.
               05  OUT-ATTR-NUMERO1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-NUMERO1-01TM        PIC X(05)       VALUE SPACES.
               05  OUT-ATTR-COMPL1-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-COMPL1-01TM         PIC X(19)       VALUE SPACES.
               05  OUT-ATTR-CIDADE1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CIDADE1-01TM        PIC X(28)       VALUE SPACES.
               05  OUT-ATTR-UF1-01TM       PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-UF1-01TM            PIC X(02)       VALUE SPACES.

               05  OUT-ATTR-CEPNRO2-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CEPNRO2-01TM.
                   07  OUT-CEPNRO2-01TM-N  PIC 9(05)       VALUE ZEROS.
               05  OUT-ATTR-CEPSUF2-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CEPSUF2-01TM.
                   07  OUT-CEPSUF2-01TM-N  PIC 9(03)       VALUE ZEROS.
               05  OUT-ATTR-CONTAT1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CONTAT1-01TM        PIC X(32)       VALUE SPACES.
               05  OUT-ATTR-ENDER2-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-ENDER2-01TM         PIC X(40)       VALUE SPACES.
               05  OUT-ATTR-NUMERO2-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-NUMERO2-01TM        PIC X(05)       VALUE SPACES.
               05  OUT-ATTR-COMPL2-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-COMPL2-01TM         PIC X(19)       VALUE SPACES.
               05  OUT-ATTR-CIDADE2-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CIDADE2-01TM        PIC X(28)       VALUE SPACES.
               05  OUT-ATTR-UF2-01TM       PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-UF2-01TM            PIC X(02)       VALUE SPACES.

               05  OUT-ATTR-TDDD1-01TM     PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-TDDD1-01TM          PIC X(04)       VALUE SPACES.
               05  OUT-ATTR-TFONE1-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-TFONE1-01TM.
                   07  OUT-TFONE1-01TM-N   PIC 9(09)       VALUE ZEROS.
               05  OUT-ATTR-FDDD1-01TM     PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-FDDD1-01TM          PIC X(04)       VALUE SPACES.
               05  OUT-ATTR-FFONE1-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-FFONE1-01TM.
                   07  OUT-FFONE1-01TM-N   PIC 9(09)       VALUE ZEROS.
               05  OUT-ATTR-CONTR1-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CONTR1-01TM.
                   07  OUT-CONTR1-01TM-N   PIC 9(05)       VALUE ZEROS.
               05  OUT-ATTR-EMAIL1-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-EMAIL1-01TM         PIC X(50)       VALUE SPACES.

               05  OUT-ATTR-CEPNRO3-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CEPNRO3-01TM.
                   07  OUT-CEPNRO3-01TM-N  PIC 9(05)       VALUE ZEROS.
               05  OUT-ATTR-CEPSUF3-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CEPSUF3-01TM.
                   07  OUT-CEPSUF3-01TM-N  PIC 9(03)       VALUE ZEROS.
               05  OUT-ATTR-ENDER3-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-ENDER3-01TM         PIC X(40)       VALUE SPACES.
               05  OUT-ATTR-NUMERO3-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-NUMERO3-01TM        PIC X(05)       VALUE SPACES.
               05  OUT-ATTR-COMPL3-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-COMPL3-01TM         PIC X(19)       VALUE SPACES.
               05  OUT-ATTR-CIDADE3-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CIDADE3-01TM        PIC X(28)       VALUE SPACES.
               05  OUT-ATTR-UF3-01TM       PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-UF3-01TM            PIC X(02)       VALUE SPACES.

               05  OUT-ATTR-PROPR1-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-PROPR1-01TM         PIC X(32)       VALUE SPACES.
               05  OUT-ATTR-CPFP1-01TM     PIC 9(04) COMP  VALUE ZEROS.
7C2511*        05  OUT-CPFP1-01TM.
7C2511*            07  OUT-CPFP1-01TM-N    PIC 9(09)       VALUE ZEROS.
7C2511         05  OUT-CPFP1-01TM.
7C2511             07  OUT-CPFP1-01TM-X    PIC X(09)       VALUE SPACES.
7C2511             07  OUT-CPFP1-01TM-N    REDEFINES OUT-CPFP1-01TM-X
7C2511                                     PIC 9(09).
               05  OUT-ATTR-DIGP1-01TM     PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-DIGP1-01TM.
                   07  OUT-DIGP1-01TM-N    PIC 9(02)       VALUE ZEROS.
               05  OUT-ATTR-DDNASC1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-DDNASC1-01TM.
                   07  OUT-DDNASC1-01TM-N  PIC X(02)       VALUE SPACES.
               05  OUT-ATTR-MMNASC1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-MMNASC1-01TM.
                   07  OUT-MMNASC1-01TM-N  PIC X(02)       VALUE SPACES.
               05  OUT-ATTR-AANASC1-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-AANASC1-01TM.
                   07  OUT-AANASC1-01TM-N  PIC X(04)       VALUE SPACES.

               05  OUT-ATTR-PROPR2-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-PROPR2-01TM         PIC X(32)       VALUE SPACES.
               05  OUT-ATTR-CPFP2-01TM     PIC 9(04) COMP  VALUE ZEROS.
7C2511*        05  OUT-CPFP2-01TM.
7C2511*            07  OUT-CPFP2-01TM-N    PIC 9(09)       VALUE ZEROS.
7C2511         05  OUT-CPFP2-01TM.
7C2511             07  OUT-CPFP2-01TM-X    PIC X(09)       VALUE SPACES.
7C2511             07  OUT-CPFP2-01TM-N    REDEFINES OUT-CPFP2-01TM-X
7C2511                                     PIC 9(09).
               05  OUT-ATTR-DIGP2-01TM     PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-DIGP2-01TM.
                   07  OUT-DIGP2-01TM-N    PIC 9(02)       VALUE ZEROS.
               05  OUT-ATTR-DDNASC2-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-DDNASC2-01TM.
                   07  OUT-DDNASC2-01TM-N  PIC X(02)       VALUE SPACES.
               05  OUT-ATTR-MMNASC2-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-MMNASC2-01TM.
                   07  OUT-MMNASC2-01TM-N  PIC X(02)       VALUE SPACES.
               05  OUT-ATTR-AANASC2-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-AANASC2-01TM.
                   07  OUT-AANASC2-01TM-N  PIC X(04)       VALUE SPACES.

               05  OUT-ATTR-PROPR3-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-PROPR3-01TM         PIC X(32)       VALUE SPACES.
               05  OUT-ATTR-CPFP3-01TM     PIC 9(04) COMP  VALUE ZEROS.
7C2511*        05  OUT-CPFP3-01TM.
7C2511*            07  OUT-CPFP3-01TM-N    PIC 9(09)       VALUE ZEROS.
7C2511         05  OUT-CPFP3-01TM.
7C2511             07  OUT-CPFP3-01TM-X    PIC X(09)       VALUE SPACES.
7C2511             07  OUT-CPFP3-01TM-N    REDEFINES OUT-CPFP3-01TM-X
7C2511                                     PIC 9(09).
               05  OUT-ATTR-DIGP3-01TM     PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-DIGP3-01TM.
                   07  OUT-DIGP3-01TM-N    PIC 9(02)       VALUE ZEROS.
               05  OUT-ATTR-DDNASC3-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-DDNASC3-01TM.
                   07  OUT-DDNASC3-01TM-N  PIC X(02)       VALUE SPACES.
               05  OUT-ATTR-MMNASC3-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-MMNASC3-01TM.
                   07  OUT-MMNASC3-01TM-N  PIC X(02)       VALUE SPACES.
               05  OUT-ATTR-AANASC3-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-AANASC3-01TM.
                   07  OUT-AANASC3-01TM-N  PIC X(04)       VALUE SPACES.

               05  OUT-ATTR-BANCO-01TM     PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-BANCO-01TM.
                   07  OUT-BANCO-01TM-N    PIC 9(03)       VALUE ZEROS.
               05  OUT-ATTR-AGENCIA-01TM   PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-AGENCIA-01TM.
                   07  OUT-AGENCIA-01TM-N  PIC 9(05)       VALUE ZEROS.
               05  OUT-ATTR-CONTA-01TM     PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-CONTA-01TM.
                   07  OUT-CONTA-01TM-N    PIC 9(07)       VALUE ZEROS.
               05  OUT-ATTR-DIGCTA-01TM    PIC 9(04) COMP  VALUE ZEROS.
               05  OUT-DIGCTA-01TM.
MARC               07  OUT-DIGCTA-01TM-N   PIC X(01)       VALUE SPACES.
MARC  *            07  OUT-DIGCTA-01TM-N   PIC 9(01).
JEM            05  WRK-OUT-INCALT-01TM     PIC X(01)       VALUE SPACE.
               05  OUT-MENSAG-01TM         PIC X(79)       VALUE SPACES.

      *----------------------------------------------------------------*
      *   AREA DA POOL0660 COM MASCARA DA TELA BVVE01TM - SEGMENTO 00  *
      *----------------------------------------------------------------*


       01  660-BVVE01TM.
           05  WRK-660-LL-AREA-01TM    PIC  9(04)  COMP  VALUE 0138.
JEM        05  WRK-660-LL-MENSAGEM-01TM PIC  9(04)  COMP  VALUE 1045.
           05  660-SENHAS-01TM         PIC  9(04)  COMP  VALUE 0037.
           05  660-NOPCAO-01TM         PIC  9(04)  COMP  VALUE 0002.
           05  660-FASE-01TM           PIC  9(04)  COMP  VALUE 0001.
           05  660-CHAMADOR-01TM       PIC  9(04)  COMP  VALUE 0008.
           05  660-PAG2660-01TM        PIC  9(04)  COMP  VALUE 0003.
JEM        05  WRK-660-ATRIBS-01TM     PIC  9(04)  COMP  VALUE 0048.
           05  660-CNPJNRO-01TM        PIC  9(04)  COMP  VALUE 0009.
           05  WRK-660-CNPJFIL-01TM    PIC  9(04)  COMP  VALUE 0007.
           05  WRK-660-CNPJCTR-01TM    PIC  9(04)  COMP  VALUE 0004.
           05  660-NOMEPRO-01TM        PIC  9(04)  COMP  VALUE 0032.
           05  660-INSCRES-01TM        PIC  9(04)  COMP  VALUE 3017.
           05  660-RAMO-01TM           PIC  9(04)  COMP  VALUE 3007.
           05  660-IRAMO-01TM          PIC  9(04)  COMP  VALUE 0040.
           05  660-NMFANT-01TM         PIC  9(04)  COMP  VALUE 2034.
           05  660-NMPLAQ-01TM         PIC  9(04)  COMP  VALUE 3024.
           05  660-CEPNRO1-01TM        PIC  9(04)  COMP  VALUE 3007.
           05  660-CEPSUF1-01TM        PIC  9(04)  COMP  VALUE 3005.
           05  660-ENDER1-01TM         PIC  9(04)  COMP  VALUE 3042.
           05  660-NUMER1-01TM         PIC  9(04)  COMP  VALUE 3007.
           05  660-COMPL1-01TM         PIC  9(04)  COMP  VALUE 3021.
           05  660-CIDADE1-01TM        PIC  9(04)  COMP  VALUE 3030.
           05  660-UF1-01TM            PIC  9(04)  COMP  VALUE 3004.
           05  660-CEPNRO2-01TM        PIC  9(04)  COMP  VALUE 3007.
           05  660-CEPSUF2-01TM        PIC  9(04)  COMP  VALUE 3005.
           05  660-CONTAT1-01TM        PIC  9(04)  COMP  VALUE 3034.
           05  660-ENDER2-01TM         PIC  9(04)  COMP  VALUE 3042.
           05  660-NUMER2-01TM         PIC  9(04)  COMP  VALUE 3007.
           05  660-COMPL2-01TM         PIC  9(04)  COMP  VALUE 3021.
           05  660-CIDADE2-01TM        PIC  9(04)  COMP  VALUE 3030.
           05  660-UF2-01TM            PIC  9(04)  COMP  VALUE 3004.
           05  660-TDDD1-01TM          PIC  9(04)  COMP  VALUE 3006.
           05  660-TFONE1-01TM         PIC  9(04)  COMP  VALUE 3011.
           05  660-FDDD1-01TM          PIC  9(04)  COMP  VALUE 3006.
           05  660-FFONE1-01TM         PIC  9(04)  COMP  VALUE 3011.
           05  660-CONTR1-01TM         PIC  9(04)  COMP  VALUE 3007.
           05  660-EMAIL1-01TM         PIC  9(04)  COMP  VALUE 3052.
           05  660-CEPNRO3-01TM        PIC  9(04)  COMP  VALUE 3007.
           05  660-CEPSUF3-01TM        PIC  9(04)  COMP  VALUE 3005.
           05  660-ENDER3-01TM         PIC  9(04)  COMP  VALUE 3042.
           05  660-NUMER3-01TM         PIC  9(04)  COMP  VALUE 3007.
           05  660-COMPL3-01TM         PIC  9(04)  COMP  VALUE 3021.
           05  660-CIDADE3-01TM        PIC  9(04)  COMP  VALUE 3030.
           05  660-UF3-01TM            PIC  9(04)  COMP  VALUE 3004.
           05  660-PROPR1-01TM         PIC  9(04)  COMP  VALUE 3034.
           05  660-CPFP1-01TM          PIC  9(04)  COMP  VALUE 3011.
           05  660-DIGP1-01TM          PIC  9(04)  COMP  VALUE 3004.
           05  660-DDNASC1-01TM        PIC  9(04)  COMP  VALUE 3004.
           05  660-MMNASC1-01TM        PIC  9(04)  COMP  VALUE 3004.
           05  660-AANASC1-01TM        PIC  9(04)  COMP  VALUE 3006.
           05  660-PROPR2-01TM         PIC  9(04)  COMP  VALUE 3034.
           05  660-CPFP2-01TM          PIC  9(04)  COMP  VALUE 3011.
           05  660-DIGP2-01TM          PIC  9(04)  COMP  VALUE 3004.
           05  660-DDNASC2-01TM        PIC  9(04)  COMP  VALUE 3004.
           05  660-MMNASC2-01TM        PIC  9(04)  COMP  VALUE 3004.
           05  660-AANASC2-01TM        PIC  9(04)  COMP  VALUE 3006.
           05  660-PROPR3-01TM         PIC  9(04)  COMP  VALUE 3034.
           05  660-CPFP3-01TM          PIC  9(04)  COMP  VALUE 3011.
           05  660-DIGP3-01TM          PIC  9(04)  COMP  VALUE 3004.
           05  660-DDNASC3-01TM        PIC  9(04)  COMP  VALUE 3004.
           05  660-MMNASC3-01TM        PIC  9(04)  COMP  VALUE 3004.
           05  660-AANASC3-01TM        PIC  9(04)  COMP  VALUE 3006.
           05  660-BANCO-01TM          PIC  9(04)  COMP  VALUE 3005.
           05  660-AGENCIA-01TM        PIC  9(04)  COMP  VALUE 3007.
           05  660-CONTA-01TM          PIC  9(04)  COMP  VALUE 3009.
           05  660-DIGCTA-01TM         PIC  9(04)  COMP  VALUE 3003.
           05  WRK-660-INCALT-01TM     PIC  9(04)  COMP  VALUE 0001.
           05  660-MENSAG-01TM         PIC  9(04)  COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)      VALUE
           'FIM    DA WORKING BVVE2610'.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  IO-PCB.
           03  IO-LTERM                PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  IO-STATUS               PIC  X(02).
           03  FILLER                  PIC  X(12).
           03  IO-MODNAME              PIC  X(08).

       01  ALT-PCB.
           03  ALT-LTERM               PIC  X(08).
           03  FILLER                  PIC  X(02).
           03  ALT-STATUS              PIC  X(02).
           03  FILLER                  PIC  X(12).
           03  ALT-MODNAME             PIC  X(08).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY   'DLITCBL'           USING   IO-PCB      ALT-PCB.

           MOVE    WRK-GU              TO      WRK-FUNCAO.

      *----------------------------------------------------------------*
       0000-OBTER-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           CALL    'POOL5000'          USING       WRK-FUNCAO
                                                   IO-PCB
                                                   ALT-PCB
                                                   WRK-MENSAGEM
                                                   WRK-TELA
                                                   WRK-NOVA-OPCAO
                                                   WRK-COD-USER
                                                   WRK-OPCAO-ATUAL
                                                   WRK-COD-DEPTO
                                                   WRK-COD-BANCO.

           IF      RETURN-CODE         EQUAL       04      OR      12
                   GOBACK.

           MOVE    SPACES              TO      OUT-DADOS-01TM
           MOVE    'BVVE01TM'          TO      WRK-TELA

           INITIALIZE          BVVE25-AREA.

           IF      WRK-TRANSACAO       NOT EQUAL       'BVVE2610'
                   PERFORM         1000-RECEBER-CONTROLE
           ELSE
                   PERFORM         2000-PROCESSAR-BVVE01TM.

           IF      WRK-TELA            EQUAL       'BVVE01TM'
                   PERFORM         3000-POOL0660-BVVE01TM.

           GO      TO      0000-OBTER-MENSAGEM.

       0000-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       1000-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MENSAGEM        TO      BVVE25-AREA.
           MOVE    BVVE25-SENHAS       TO      INP-SENHAS-01TM.

           MOVE    '1'                 TO      INP-FASE-01TM.
JEM        MOVE    ' '                 TO      WRK-INP-INCALT-01TM.
7C2511*    MOVE    BVVE25-NUMERO       TO      INP-CNPJNRO-01TM-N.
7C2511*    MOVE    BVVE25-FILIAL       TO      INP-CNPJFIL-01TM-N.
7C2511     MOVE    BVVE25-NUMERO       TO      INP-CNPJNRO-01TM-X.
7C2511     MOVE    BVVE25-FILIAL       TO      INP-CNPJFIL-01TM-X.
           MOVE    BVVE25-CONTROLE     TO      INP-CNPJCTR-01TM-N.
           MOVE    BVVE25-NOME         TO      INP-NOMEPRO-01TM.
           MOVE    BVVE25-RAMO         TO      INP-RAMO-01TM-N.
           MOVE    BVVE25-DESCR-RAMO   TO      INP-IRAMO-01TM
           MOVE    WRK-TRANSACAO       TO      INP-CHAMADOR-01TM.

           IF      WRK-TRANSACAO       EQUAL   'BVVE2660'
      *************MOVE    BVVE25-FILLER(1:3)  TO   INP-PAG2660-01TM-N
                   MOVE    BVVE25-FILLER(1:3)  TO   WRK-PAGINA
                   MOVE    WRK-PAGINA-R        TO   INP-PAG2660-01TM-N
           END-IF.

JEM        MOVE 'S'                    TO WRK-RECEBER
JEM        PERFORM         1200-MONTAR-TELA.
JEM        MOVE 'N'                    TO WRK-RECEBER.

       1000-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       1200-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-COD-BANCO       TO      WRK-E-BANCO-CLIE
           MOVE    WRK-COD-DEPTO-N     TO      WRK-E-AGENCIA-CLIE
           MOVE    ZEROS               TO      WRK-E-CONTA-CLIE
           MOVE    ZEROS               TO      WRK-E-DIG-CONTA-CLIE

           PERFORM 1220-ACESSAR-BVVE7008.

           IF      BVVE21-COD-RETORNO          EQUAL      03
                   MOVE    WRK-MSG03           TO      BVVE25-MENSAGEM
                   PERFORM             2100-TRATAR-PFK03
                   GO          TO      1200-99-FIM
           END-IF.

           PERFORM         1240-CARREGA-CAMPOS.

           PERFORM         2400-DEVOLVE-TELA.

       1200-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       1220-ACESSAR-BVVE7008           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVE21-AREA-ENTRADA
                       BVVE22-AREA-VARIAVEIS.

7C2511*    MOVE    INP-CNPJNRO-01TM-N      TO      WRK-E-NUMERO-CLIE
7C2511*    MOVE    INP-CNPJFIL-01TM-N      TO      WRK-E-FILIAL-CLIE
7C2511     MOVE    INP-CNPJNRO-01TM-X      TO      WRK-E-NUMERO-CLIE
7C2511     MOVE    INP-CNPJFIL-01TM-X      TO      WRK-E-FILIAL-CLIE
           MOVE    INP-CNPJCTR-01TM-N      TO      WRK-E-CONTROLE-CLIE
JEM
           IF  WRK-RECEBER   EQUAL 'S'    AND
JEM            BVVE25-DIRETO EQUAL  2
JEM            MOVE 1                      TO WRK-E-FILIAL-CLIE
JEM            MOVE BVVE25-CCTRL-MATRIZ    TO WRK-E-CONTROLE-CLIE
JEM        END-IF

7C2511*    IF      INP-CNPJFIL-01TM-N      NOT EQUAL       ZEROS
7C2511     IF      INP-CNPJFIL-01TM-X      NOT EQUAL       SPACES AND
7C2511             INP-CNPJFIL-01TM-X      NOT EQUAL       LOW-VALUES
                   MOVE    1               TO      WRK-E-TIPO-CLIE
           ELSE
                   MOVE    2               TO      WRK-E-TIPO-CLIE
           END-IF.

           MOVE    'OPRD'                  TO      BVVE21-COMANDO.
           MOVE    'BVVE7008'              TO      BVVE21-MODULO.
           MOVE    WRK-ENVIA-DADOS-CLIENTE TO      BVVE21-PARAMETROS.
           MOVE    368                     TO      BVVE21-TAM-REGISTRO.

           MOVE    'BVVE7920'              TO      WRK-MODULO.

           CALL    WRK-MODULO      USING     BVVE21-AREA-ENTRADA
                                             BVVE22-AREA-VARIAVEIS
                                             ERRO-AREA
                                             WRK-SQLCA
                                             WRK-RECEBE-DADOS-CLIENTE.

           IF      BVVE21-COD-RETORNO      NOT EQUAL       ZEROS    AND
                   BVVE21-COD-RETORNO      NOT EQUAL       01       AND
                   BVVE21-COD-RETORNO      NOT EQUAL       02       AND
                   BVVE21-COD-RETORNO      NOT EQUAL       03
                   MOVE BVVE21-MODULO      TO ERR-MODULO
                   PERFORM     9999-ROTINA-ERRO
           END-IF.

           IF      BVVE21-COD-RETORNO          EQUAL       01  AND
MARC  *            IF ERR-TEXTO(01:10) EQUAL 'LOCAL 1.3-'
MARC  *               MOVE 'CONTA NAO CONFERE PARA ESTE CPF/CNPJ'
MARC  *                                    TO OUT-MENSAG-01TM
MARC  *               MOVE OUT-CONTA-01TM  TO INP-CONTA-01TM
MARC  ****************MOVE WRK-49353          TO INP-ATR-CONTA
MARC  ****************PERFORM              2400-DEVOLVE-TELA
MARC  *               MOVE  'BVVE01TM'     TO WRK-TELA
MARC  ****************PERFORM  3000-POOL0660-BVVE01TM
MARC  ****************GO TO 0000-OBTER-MENSAGEM
MARC  *            END-IF
                   ERR-TIPO-ACESSO         NOT EQUAL       'APL'
                   MOVE BVVE21-MODULO      TO ERR-MODULO
                   PERFORM     9999-ROTINA-ERRO
           END-IF.

       1220-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       1230-ACESSAR-BVVE7010           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVE21-AREA-ENTRADA
                       BVVE22-AREA-VARIAVEIS.

           MOVE    'OPRD'                  TO      BVVE21-COMANDO.
           MOVE    'BVVE7010'              TO      BVVE21-MODULO.
           MOVE    WRK-ENVIA-DADOS-RAMO    TO      BVVE21-PARAMETROS.
           MOVE    045                     TO      BVVE21-TAM-REGISTRO.

           MOVE    'BVVE7920'              TO      WRK-MODULO.

           CALL    WRK-MODULO      USING     BVVE21-AREA-ENTRADA
                                             BVVE22-AREA-VARIAVEIS
                                             ERRO-AREA
                                             WRK-SQLCA
                                             WRK-RECEBE-DADOS-RAMO.

           IF      BVVE21-COD-RETORNO      NOT EQUAL       ZEROS    AND
                   BVVE21-COD-RETORNO      NOT EQUAL       03
                   MOVE BVVE21-MODULO      TO ERR-MODULO
                   PERFORM     9999-ROTINA-ERRO
           END-IF.

       1230-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       1240-CARREGA-CAMPOS             SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-S-NOME-CLIE         TO INP-NOMEPRO-01TM

           MOVE  WRK-S-CEP-CLIE          TO INP-CEPNRO1-01TM-N
           MOVE  WRK-S-CEP-COMPL-CLIE    TO INP-CEPSUF1-01TM-N
           MOVE  WRK-S-ENDER-CLIE        TO INP-ENDER1-01TM
MARC       MOVE  WRK-S-NR-ENDER-CLIE     TO INP-NUMERO1-01TM
           MOVE  WRK-S-COMPL-CLIE        TO INP-COMPL1-01TM
           MOVE  WRK-S-CIDADE-CLIE       TO INP-CIDADE1-01TM
           MOVE  WRK-S-UF-CLIE           TO INP-UF1-01TM

           MOVE  WRK-S-CEP-CLIE          TO INP-CEPNRO2-01TM-N
           MOVE  WRK-S-CEP-COMPL-CLIE    TO INP-CEPSUF2-01TM-N
           MOVE  WRK-S-ENDER-CLIE        TO INP-ENDER2-01TM
MARC       MOVE  WRK-S-NR-ENDER-CLIE     TO INP-NUMERO2-01TM
           MOVE  WRK-S-COMPL-CLIE        TO INP-COMPL2-01TM
           MOVE  WRK-S-CIDADE-CLIE       TO INP-CIDADE2-01TM
           MOVE  WRK-S-UF-CLIE           TO INP-UF2-01TM

           MOVE  WRK-S-CEP-CLIE          TO INP-CEPNRO3-01TM-N
           MOVE  WRK-S-CEP-COMPL-CLIE    TO INP-CEPSUF3-01TM-N
           MOVE  WRK-S-ENDER-CLIE        TO INP-ENDER3-01TM
MARC       MOVE  WRK-S-NR-ENDER-CLIE     TO INP-NUMERO3-01TM
           MOVE  WRK-S-COMPL-CLIE        TO INP-COMPL3-01TM
           MOVE  WRK-S-CIDADE-CLIE       TO INP-CIDADE3-01TM
           MOVE  WRK-S-UF-CLIE           TO INP-UF3-01TM

           MOVE  WRK-S-DDD-FONE-CLIE     TO INP-TDDD1-01TM
           MOVE  WRK-S-FONE-CLIE         TO INP-TFONE1-01TM-N

           MOVE  WRK-S-PROPR1-CLIE       TO INP-PROPR1-01TM
7C2511*    MOVE  WRK-S-CPF1-CLIE         TO INP-CPFP1-01TM-N
7C2511     MOVE  WRK-S-CPF1-CLIE         TO INP-CPFP1-01TM-X
           MOVE  WRK-S-CTRLCPF1-CLIE     TO INP-DIGP1-01TM-N

           MOVE  WRK-S-PROPR2-CLIE       TO INP-PROPR2-01TM
           IF    WRK-S-CPF2-CLIE       =    ZEROS
                 MOVE  SPACES               TO INP-CPFP2-01TM
                 MOVE  SPACES               TO INP-DIGP2-01TM
           ELSE
7C2511*          MOVE  WRK-S-CPF2-CLIE      TO INP-CPFP2-01TM-N
7C2511           MOVE  WRK-S-CPF2-CLIE      TO INP-CPFP2-01TM-X
                 MOVE  WRK-S-CTRLCPF2-CLIE  TO INP-DIGP2-01TM-N
           END-IF

           MOVE  WRK-S-PROPR3-CLIE          TO INP-PROPR3-01TM
           IF    WRK-S-CPF3-CLIE       =    ZEROS
                 MOVE  SPACES               TO INP-CPFP3-01TM
                 MOVE  SPACES               TO INP-DIGP3-01TM
           ELSE
7C2511*          MOVE  WRK-S-CPF3-CLIE      TO INP-CPFP3-01TM-N
7C2511           MOVE  WRK-S-CPF3-CLIE      TO INP-CPFP3-01TM-X
                 MOVE  WRK-S-CTRLCPF3-CLIE  TO INP-DIGP3-01TM-N
           END-IF

           MOVE  WRK-S-CBCO                 TO INP-BANCO-01TM-N
           MOVE  WRK-S-AG-BCARIA            TO INP-AGENCIA-01TM-N.
           MOVE  WRK-S-CCTA-BCARIA          TO INP-CONTA-01TM-N.

MARC  *    MOVE  WRK-S-DIG-CCTA-BCARIA      TO INP-DIGCTA-01TM-N.


MARC       IF  WRK-S-DIG-CCTA-BCARIA        EQUAL ZEROS
MARC           MOVE    INP-CONTA-01TM-N     TO WRK-CAMPO-DIG
MARC           MOVE    07                   TO WRK-TAMANHO-DIG
MARC
MARC           CALL    'POOL0431'           USING WRK-CAMPO-DIG
MARC                                              WRK-DIGITO-DIG
MARC                                              WRK-TAMANHO-DIG
MARC
MARC           IF  RETURN-CODE              NOT EQUAL       ZEROS
MARC               MOVE    RETURN-CODE      TO WRK-MSG10-RET-CODE
MARC               MOVE    'POOL0431'       TO WRK-MSG10-NM-POOL
MARC               MOVE    WRK-MSG10        TO OUT-MENSAG-01TM
MARC               MOVE    'S'              TO WRK-INCONSISTENTE
MARC               GO      TO               1240-99-FIM
MARC           END-IF
MARC
MARC           IF  WRK-DIGITO-DIG           EQUAL  '.'
MARC               MOVE    'S'              TO WRK-INCONSISTENTE
MARC               MOVE    WRK-49353           TO OUT-ATTR-DIGCTA-01TM
MARC               MOVE    WRK-MSG18        TO OUT-MENSAG-01TM
MARC               GO      TO               1240-99-FIM
MARC           END-IF
MARC
MARC           MOVE  WRK-DIGITO-DIG         TO INP-DIGCTA-01TM-N
MARC       ELSE
MARC           MOVE  WRK-S-DIG-CCTA-BCARIA  TO INP-DIGCTA-01TM-N
MARC       END-IF.

           PERFORM         1241-DESPROTEGER-CAMPOS.

       1240-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       1241-DESPROTEGER-CAMPOS         SECTION.
      *----------------------------------------------------------------*
           MOVE   WRK-49361            TO    OUT-ATTR-INSCRES-01TM
           MOVE   WRK-225              TO    OUT-ATTR-RAMO-01TM
           MOVE   WRK-193              TO    OUT-ATTR-BANCO-01TM
                                             OUT-ATTR-CEPNRO1-01TM
                                             OUT-ATTR-CEPSUF1-01TM
                                             OUT-ATTR-ENDER1-01TM
                                             OUT-ATTR-NUMERO1-01TM
                                             OUT-ATTR-COMPL1-01TM
                                             OUT-ATTR-CIDADE1-01TM
                                             OUT-ATTR-UF1-01TM
                                             WRK-OUT-ATTR-CNPJCTR-01TM
                                             OUT-ATTR-NMFANT-01TM
                                             OUT-ATTR-NMPLAQ-01TM
                                             OUT-ATTR-CEPNRO2-01TM
                                             OUT-ATTR-CEPSUF2-01TM
                                             OUT-ATTR-NUMERO2-01TM
                                             OUT-ATTR-ENDER2-01TM
                                             OUT-ATTR-COMPL2-01TM
                                             OUT-ATTR-CIDADE2-01TM
                                             OUT-ATTR-UF2-01TM
                                             OUT-ATTR-TDDD1-01TM
                                             OUT-ATTR-TFONE1-01TM
                                             OUT-ATTR-FDDD1-01TM
                                             OUT-ATTR-FFONE1-01TM
                                             OUT-ATTR-CONTR1-01TM
                                             OUT-ATTR-CONTAT1-01TM
                                             OUT-ATTR-EMAIL1-01TM
                                             OUT-ATTR-CEPNRO3-01TM
                                             OUT-ATTR-CEPSUF3-01TM
                                             OUT-ATTR-NUMERO3-01TM
                                             OUT-ATTR-ENDER3-01TM
                                             OUT-ATTR-COMPL3-01TM
                                             OUT-ATTR-CIDADE3-01TM
                                             OUT-ATTR-UF3-01TM
                                             OUT-ATTR-PROPR1-01TM
                                             OUT-ATTR-CPFP1-01TM
                                             OUT-ATTR-DIGP1-01TM
                                             OUT-ATTR-DDNASC1-01TM
                                             OUT-ATTR-MMNASC1-01TM
                                             OUT-ATTR-AANASC1-01TM
                                             OUT-ATTR-PROPR2-01TM
                                             OUT-ATTR-CPFP2-01TM
                                             OUT-ATTR-DIGP2-01TM
                                             OUT-ATTR-DDNASC2-01TM
                                             OUT-ATTR-MMNASC2-01TM
                                             OUT-ATTR-AANASC2-01TM
                                             OUT-ATTR-PROPR3-01TM
                                             OUT-ATTR-CPFP3-01TM
                                             OUT-ATTR-DIGP3-01TM
                                             OUT-ATTR-DDNASC3-01TM
                                             OUT-ATTR-MMNASC3-01TM
                                             OUT-ATTR-AANASC3-01TM
                                             OUT-ATTR-BANCO-01TM
                                             OUT-ATTR-AGENCIA-01TM
                                             OUT-ATTR-CONTA-01TM
                                             OUT-ATTR-DIGCTA-01TM.

           IF    INP-AGENCIA-01TM-N  EQUAL  WRK-COD-DEPTO-N
                 MOVE   WRK-225        TO   OUT-ATTR-AGENCIA-01TM
           END-IF.

       1241-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2000-PROCESSAR-BVVE01TM         SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MENSAGEM        TO      INPUT-01TM

           IF      INP-PFK-01TM                EQUAL 'H'
                   MOVE    INP-MENSAG-01TM     TO      OUT-MENSAG-01TM
                   PERFORM     2400-DEVOLVE-TELA
JEM                 GO                         TO 2000-99-FIM
           END-IF.

           IF      INP-PFK-01TM                EQUAL 'B'
                   PERFORM     2100-TRATAR-PFK03
JEM                 GO                         TO 2000-99-FIM
           END-IF.

           IF INP-PFK-01TM         EQUAL '.'
              IF INP-FASE-01TM     EQUAL '1' OR '2'
                 PERFORM 2200-TRATAR-ENTER
                 GO                    TO 2000-99-FIM
              ELSE
                 MOVE WRK-MSG01        TO OUT-MENSAG-01TM
                 PERFORM 2400-DEVOLVE-TELA
                 GO                    TO 2000-99-FIM
              END-IF
           END-IF.

           IF INP-PFK-01TM         EQUAL '6'
              IF INP-FASE-01TM     EQUAL '2'
                 PERFORM 2300-TRATAR-PFK06
                 GO                    TO 2000-99-FIM
              ELSE
                 IF INP-FASE-01TM  EQUAL '3'
                    PERFORM 2400-DEVOLVE-TELA
                    MOVE WRK-MSG07     TO OUT-MENSAG-01TM
                    GO                 TO 2000-99-FIM
                 ELSE
                    MOVE WRK-MSG01     TO OUT-MENSAG-01TM
                    PERFORM 2400-DEVOLVE-TELA
                    GO                 TO 2000-99-FIM
                 END-IF
              END-IF
           END-IF.

JEM        IF      INP-PFK-01TM         EQUAL 'C' AND
JEM                WRK-INP-INCALT-01TM  EQUAL '*'
JEM                IF INP-FASE-01TM     EQUAL '2'
JEM                   PERFORM 2301-TRATAR-PFK11
JEM                   GO                    TO 2000-99-FIM
JEM                ELSE
JEM                   IF INP-FASE-01TM  EQUAL '3'
JEM                      PERFORM 2400-DEVOLVE-TELA
JEM                      MOVE WRK-MSG07     TO OUT-MENSAG-01TM
JEM                      GO                 TO 2000-99-FIM
JEM                   ELSE
JEM                      MOVE WRK-MSG01     TO OUT-MENSAG-01TM
JEM                      PERFORM 2400-DEVOLVE-TELA
JEM                      GO                 TO 2000-99-FIM
JEM                   END-IF
JEM                END-IF
JEM        ELSE
JEM                MOVE WRK-MSG01           TO OUT-MENSAG-01TM
JEM                PERFORM 2400-DEVOLVE-TELA
JEM        END-IF.

       2000-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2100-TRATAR-PFK03               SECTION.
      *----------------------------------------------------------------*

           MOVE    +186                TO      BVVE25-LL.
           MOVE    INP-SENHAS-01TM     TO      BVVE25-SENHAS.
           MOVE    INP-CHAMADOR-01TM   TO      WRK-TELA.
7C2511*    MOVE    INP-CNPJNRO-01TM-N  TO      BVVE25-NUMERO.
7C2511*    MOVE    INP-CNPJFIL-01TM-N  TO      BVVE25-FILIAL
7C2511     MOVE    INP-CNPJNRO-01TM-X  TO      BVVE25-NUMERO.
7C2511     MOVE    INP-CNPJFIL-01TM-X  TO      BVVE25-FILIAL
           MOVE    INP-CNPJCTR-01TM-N  TO      BVVE25-CONTROLE.
           MOVE    INP-NOMEPRO-01TM    TO      BVVE25-NOME.
           MOVE    INP-RAMO-01TM-N     TO      BVVE25-RAMO.
           MOVE    INP-IRAMO-01TM      TO      BVVE25-DESCR-RAMO.
           MOVE    'BVVE2610'          TO      BVVE25-TRANCODE.

           IF      INP-CHAMADOR-01TM       EQUAL       'BVVE2660'
      *************MOVE  INP-PAG2660-01TM-N  TO  BVVE25-FILLER(1:3)
                   MOVE  INP-PAG2660-01TM-N  TO  WRK-PAGINA-R
                   MOVE  WRK-PAGINA          TO  BVVE25-FILLER(1:3)
           END-IF.

           MOVE    WRK-CHNG            TO      WRK-FUNCAO

           MOVE    BVVE25-AREA         TO      WRK-MENSAGEM.

       2100-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2200-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF      INP-FASE-01TM       EQUAL       '1'
                   PERFORM         2210-TRATAR-FASE-1
                   PERFORM         2400-DEVOLVE-TELA
           ELSE
             IF    INP-FASE-01TM       EQUAL       '2'
                   MOVE    '1'             TO      INP-FASE-01TM
                   PERFORM         1241-DESPROTEGER-CAMPOS
                   PERFORM         2400-DEVOLVE-TELA
             END-IF
           END-IF.

       2200-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2210-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           PERFORM         2211-CONSISTENCIA-FISICA

           IF      WRK-INCONSISTENTE       NOT EQUAL       SPACES
                   GO      TO      2210-99-FIM
           END-IF.

           PERFORM         2221-CONSISTENCIA-FISICA2.

           IF      WRK-INCONSISTENTE       NOT EQUAL       SPACES
                   GO      TO      2210-99-FIM
           END-IF.

           PERFORM         2231-CONSISTENCIA-LOGICA.

           IF      WRK-INCONSISTENTE       NOT EQUAL       SPACES
                   GO      TO      2210-99-FIM
           END-IF.

           MOVE    '2'                 TO          INP-FASE-01TM.

           PERFORM         2241-PROTEGER-CAMPOS.

7C2511*    IF  INP-CNPJFIL-01TM    NOT EQUAL ZEROS
7C2511     IF  INP-CNPJFIL-01TM-X  NOT EQUAL SPACES AND
7C2511         INP-CNPJFIL-01TM-X  NOT EQUAL LOW-VALUES
JEM            MOVE '*'                TO WRK-INP-INCALT-01TM
JEM            MOVE WRK-MSG42          TO OUT-MENSAG-01TM
JEM        ELSE
JEM            MOVE ' '                TO WRK-INP-INCALT-01TM
JEM            MOVE WRK-MSG05          TO OUT-MENSAG-01TM
JEM        END-IF.

       2210-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2211-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-193              TO     OUT-ATTR-BANCO-01TM
                                              OUT-ATTR-CEPNRO1-01TM
                                              OUT-ATTR-CEPSUF1-01TM
                                              OUT-ATTR-ENDER1-01TM
                                              OUT-ATTR-NUMERO1-01TM
                                              OUT-ATTR-COMPL1-01TM
                                              OUT-ATTR-CIDADE1-01TM
                                              OUT-ATTR-UF1-01TM
                                              OUT-ATTR-ENDER2-01TM
                                              OUT-ATTR-ENDER3-01TM
                                              OUT-ATTR-INSCRES-01TM
                                              OUT-ATTR-NMFANT-01TM
                                              OUT-ATTR-NMPLAQ-01TM
                                              OUT-ATTR-TDDD1-01TM
                                              OUT-ATTR-TFONE1-01TM
                                              OUT-ATTR-FDDD1-01TM
                                              OUT-ATTR-FFONE1-01TM
                                              OUT-ATTR-CONTR1-01TM
                                              OUT-ATTR-CONTAT1-01TM
                                              OUT-ATTR-EMAIL1-01TM
                                              OUT-ATTR-CEPNRO2-01TM
                                              OUT-ATTR-CEPSUF2-01TM
                                              OUT-ATTR-NUMERO2-01TM
                                              OUT-ATTR-COMPL2-01TM
                                              OUT-ATTR-CIDADE2-01TM
                                              OUT-ATTR-UF2-01TM
                                              OUT-ATTR-CEPNRO3-01TM
                                              OUT-ATTR-CEPSUF3-01TM
                                              OUT-ATTR-NUMERO3-01TM
                                              OUT-ATTR-COMPL3-01TM
                                              OUT-ATTR-CIDADE3-01TM
                                              OUT-ATTR-UF3-01TM
                                              OUT-ATTR-PROPR1-01TM
                                              OUT-ATTR-CPFP1-01TM
                                              OUT-ATTR-DIGP1-01TM
                                              OUT-ATTR-DDNASC1-01TM
                                              OUT-ATTR-MMNASC1-01TM
                                              OUT-ATTR-AANASC1-01TM
                                              OUT-ATTR-PROPR2-01TM
                                              OUT-ATTR-CPFP2-01TM
                                              OUT-ATTR-CPFP2-01TM
                                              OUT-ATTR-DDNASC2-01TM
                                              OUT-ATTR-MMNASC2-01TM
                                              OUT-ATTR-AANASC2-01TM
                                              OUT-ATTR-PROPR3-01TM
                                              OUT-ATTR-CPFP3-01TM
                                              OUT-ATTR-CPFP3-01TM
                                              OUT-ATTR-DDNASC3-01TM
                                              OUT-ATTR-MMNASC3-01TM
                                              OUT-ATTR-AANASC3-01TM
                                              OUT-ATTR-AGENCIA-01TM
                                              OUT-ATTR-CONTA-01TM
                                              OUT-ATTR-DIGCTA-01TM.

           PERFORM         2212-FAZER-INSPECT.

           MOVE    SPACES              TO         WRK-INCONSISTENTE
                                                  WRK-ERR-NUM-ENDERECO.

           PERFORM         2213-CONSISTE-ERRO.

           IF WRK-INCONSISTENTE        NOT EQUAL       SPACES
JEM           IF  WRK-INCONSISTENTE    NOT EQUAL 'D'
JEM               MOVE WRK-MSG08       TO OUT-MENSAG-01TM
JEM           END-IF
1214          IF  WRK-ERR-NUM-ENDERECO EQUAL 'S'
1214              MOVE WRK-MSG21       TO OUT-MENSAG-01TM
1214          END-IF
              PERFORM 2400-DEVOLVE-TELA
           END-IF.

       2211-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2212-FAZER-INSPECT              SECTION.
      *----------------------------------------------------------------*

           IF      INP-INSCRES-01TM    NOT EQUAL       SPACES      AND
                   INP-INSCRES-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-INSCRES-01TM    TO      OUT-INSCRES-01TM
                   INSPECT INP-INSCRES-01TM            REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-RAMO-01TM       NOT EQUAL       SPACES      AND
                   INP-RAMO-01TM       NOT EQUAL       LOW-VALUES
                   MOVE    INP-RAMO-01TM       TO      OUT-RAMO-01TM
                   INSPECT     INP-RAMO-01TM           REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-CEPNRO2-01TM    NOT EQUAL       SPACES      AND
                   INP-CEPNRO2-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-CEPNRO2-01TM    TO      OUT-CEPNRO2-01TM
                   INSPECT     INP-CEPNRO2-01TM        REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-CEPSUF2-01TM    NOT EQUAL       SPACES      AND
                   INP-CEPSUF2-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-CEPSUF2-01TM    TO      OUT-CEPSUF2-01TM
                   INSPECT     INP-CEPSUF2-01TM        REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-TDDD1-01TM      NOT EQUAL       SPACES      AND
                   INP-TDDD1-01TM      NOT EQUAL       LOW-VALUES
                   MOVE    INP-TDDD1-01TM      TO      OUT-TDDD1-01TM
                   INSPECT     INP-TDDD1-01TM          REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-TFONE1-01TM     NOT EQUAL       SPACES      AND
                   INP-TFONE1-01TM     NOT EQUAL       LOW-VALUES
                   MOVE    INP-TFONE1-01TM     TO      OUT-TFONE1-01TM
                   INSPECT     INP-TFONE1-01TM         REPLACING
                           LEADING         ' '         BY          '0'.

           MOVE    INP-FDDD1-01TM      TO      OUT-FDDD1-01TM
           INSPECT     INP-FDDD1-01TM          REPLACING
                        LEADING         ' '         BY          '0'.

           MOVE    INP-FFONE1-01TM     TO      OUT-FFONE1-01TM
           INSPECT     INP-FFONE1-01TM         REPLACING
                        LEADING         ' '         BY          '0'.

           IF      INP-CONTR1-01TM     NOT EQUAL       SPACES      AND
                   INP-CONTR1-01TM     NOT EQUAL       LOW-VALUES
                   MOVE    INP-CONTR1-01TM     TO      OUT-CONTR1-01TM
                   INSPECT     INP-CONTR1-01TM         REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-CEPNRO3-01TM    NOT EQUAL       SPACES      AND
                   INP-CEPNRO3-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-CEPNRO3-01TM    TO      OUT-CEPNRO3-01TM
                   INSPECT     INP-CEPNRO3-01TM        REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-CEPSUF3-01TM    NOT EQUAL       SPACES      AND
                   INP-CEPSUF3-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-CEPSUF3-01TM    TO      OUT-CEPSUF3-01TM

                   INSPECT     INP-CEPSUF3-01TM        REPLACING
                           LEADING         ' '         BY          '0'.

7C2511*    IF      INP-CNPJFIL-01TM-N      NOT EQUAL       ZEROS
           IF      INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
                   INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
             IF    INP-CPFP1-01TM          NOT EQUAL       SPACES  AND
                   INP-CPFP1-01TM          NOT EQUAL       LOW-VALUES
                   MOVE    INP-CPFP1-01TM    TO      OUT-CPFP1-01TM
                   INSPECT     INP-CPFP1-01TM        REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-DIGP1-01TM          NOT EQUAL       SPACES  AND
                   INP-DIGP1-01TM          NOT EQUAL       LOW-VALUES
                   MOVE    INP-DIGP1-01TM    TO      OUT-DIGP1-01TM
                   INSPECT     INP-DIGP1-01TM        REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-DDNASC1-01TM        NOT EQUAL       SPACES  AND
                   INP-DDNASC1-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-DDNASC1-01TM  TO      OUT-DDNASC1-01TM
                   INSPECT     INP-DDNASC1-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-MMNASC1-01TM        NOT EQUAL       SPACES  AND
                   INP-MMNASC1-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-MMNASC1-01TM  TO      OUT-MMNASC1-01TM
                   INSPECT     INP-MMNASC1-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-AANASC1-01TM        NOT EQUAL       SPACES  AND
                   INP-AANASC1-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-AANASC1-01TM  TO      OUT-AANASC1-01TM
                   INSPECT     INP-AANASC1-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-CPFP2-01TM          NOT EQUAL       SPACES  AND
                   INP-CPFP2-01TM          NOT EQUAL       LOW-VALUES
                   MOVE    INP-CPFP2-01TM    TO      OUT-CPFP2-01TM
                   INSPECT     INP-CPFP2-01TM        REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-DIGP2-01TM          NOT EQUAL       SPACES  AND
                   INP-DIGP2-01TM          NOT EQUAL       LOW-VALUES
                   MOVE    INP-DIGP2-01TM    TO      OUT-DIGP2-01TM
                   INSPECT     INP-DIGP2-01TM        REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-DDNASC2-01TM        NOT EQUAL       SPACES  AND
                   INP-DDNASC2-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-DDNASC2-01TM  TO      OUT-DDNASC2-01TM
                   INSPECT     INP-DDNASC2-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-MMNASC2-01TM        NOT EQUAL       SPACES  AND
                   INP-MMNASC2-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-MMNASC2-01TM  TO      OUT-MMNASC2-01TM
                   INSPECT     INP-MMNASC2-01TM      REPLACING
                           LEADING         ' '         BY          '0'

             END-IF
             IF    INP-AANASC2-01TM        NOT EQUAL       SPACES  AND
                   INP-AANASC2-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-AANASC2-01TM  TO      OUT-AANASC2-01TM
                   INSPECT     INP-AANASC2-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-CPFP3-01TM          NOT EQUAL       SPACES  AND
                   INP-CPFP3-01TM          NOT EQUAL       LOW-VALUES
                   MOVE    INP-CPFP3-01TM    TO      OUT-CPFP3-01TM
                   INSPECT     INP-CPFP3-01TM        REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-DIGP3-01TM          NOT EQUAL       SPACES  AND
                   INP-DIGP3-01TM          NOT EQUAL       LOW-VALUES
                   MOVE    INP-DIGP3-01TM    TO      OUT-DIGP3-01TM
                   INSPECT     INP-DIGP3-01TM        REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-DDNASC3-01TM        NOT EQUAL       SPACES  AND
                   INP-DDNASC3-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-DDNASC3-01TM  TO      OUT-DDNASC3-01TM
                   INSPECT     INP-DDNASC3-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-MMNASC3-01TM        NOT EQUAL       SPACES  AND
                   INP-MMNASC3-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-MMNASC3-01TM  TO      OUT-MMNASC3-01TM
                   INSPECT     INP-MMNASC3-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
             IF    INP-AANASC3-01TM        NOT EQUAL       SPACES  AND
                   INP-AANASC3-01TM        NOT EQUAL       LOW-VALUES
                   MOVE    INP-AANASC3-01TM  TO      OUT-AANASC3-01TM
                   INSPECT     INP-AANASC3-01TM      REPLACING
                           LEADING         ' '         BY          '0'
             END-IF
           END-IF.

           IF      INP-BANCO-01TM    NOT EQUAL       SPACES      AND
                   INP-BANCO-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-BANCO-01TM    TO      OUT-BANCO-01TM
                   INSPECT     INP-BANCO-01TM        REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-AGENCIA-01TM    NOT EQUAL       SPACES    AND
                   INP-AGENCIA-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-AGENCIA-01TM    TO      OUT-AGENCIA-01TM
                   INSPECT     INP-AGENCIA-01TM        REPLACING
                           LEADING         ' '         BY          '0'.

           IF      INP-CONTA-01TM    NOT EQUAL       SPACES      AND
                   INP-CONTA-01TM    NOT EQUAL       LOW-VALUES
                   MOVE    INP-CONTA-01TM    TO      OUT-CONTA-01TM
                   INSPECT     INP-CONTA-01TM        REPLACING
                           LEADING         ' '         BY          '0'.

       2212-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2213-CONSISTE-ERRO              SECTION.
      *----------------------------------------------------------------*

JEM        IF  WRK-INP-INCALT-01TM EQUAL '*'
JEM            IF  INP-CNPJFIL-01TM NOT NUMERIC OR
JEM                INP-CNPJFIL-01TM EQUAL ZEROS
                   MOVE WRK-49353      TO WRK-OUT-ATTR-CNPJFIL-01TM
JEM                MOVE 'S'            TO WRK-INCONSISTENTE
JEM                MOVE WRK-MSG43      TO OUT-MENSAG-01TM
JEM                GO                  TO 2213-99-FIM
JEM            END-IF
JEM            IF  INP-CNPJCTR-01TM NOT NUMERIC OR
JEM                INP-CNPJCTR-01TM EQUAL ZEROS
                   MOVE WRK-49353         TO WRK-OUT-ATTR-CNPJCTR-01TM
JEM                MOVE 'S'            TO WRK-INCONSISTENTE
JEM                MOVE WRK-MSG43      TO OUT-MENSAG-01TM
JEM                GO                  TO 2213-99-FIM
JEM            END-IF
JEM        END-IF.
JEM
7C2511*        IF  WRK-INP-INCALT-01TM EQUAL '*'
7C2511*            MOVE  INP-CNPJNRO-01TM-N TO WRK-CPF-CGC-P
7C2511*            MOVE  INP-CNPJFIL-01TM-N TO WRK-FILIAL-P
7C2511*            CALL  'POOL0110'        USING WRK-CGC-FILIAL-P
7C2511*                                          WRK-CONTROLE-P
7C2511*            IF  RETURN-CODE         NOT EQUAL ZEROS OR
7C2511*                WRK-CONTROLE-P      NOT EQUAL INP-CNPJCTR-01TM-N
7C2511*                   MOVE WRK-49353         TO WRK-OUT-ATTR-CNPJCTR-01TM
7C2511*                MOVE 'S'            TO WRK-INCONSISTENTE
7C2511*                MOVE WRK-MSG43      TO OUT-MENSAG-01TM
7C2511*                GO                  TO 2213-99-FIM
7C2511*            END-IF
7C2511*        END-IF.

7C2511         IF WRK-INP-INCALT-01TM EQUAL '*'
7C2511*            MOVIMENTAÇÕES PARA OS CAMPOS DA BRAD2000
7C2511             MOVE ZEROS          TO WRK-CAD-CONTROLE.
7C2511             MOVE INP-CNPJNRO-01TM-X TO WRK-CAD-NUMERO.
7C2511             MOVE INP-CNPJFIL-01TM-X TO WRK-CAD-FILIAL.

7C2511*            CHAMADA DA BRAD2000
7C2511             CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
7C2511                                         WRK-CAD-CONTROLE.

7C2511*            VALIDACAO DO RETORNO DA CHAMADA
7C2511             IF RETURN-CODE NOT EQUAL ZEROS
7C2511                 MOVE WRK-49353 TO WRK-OUT-ATTR-CNPJCTR-01TM
7C2511                 MOVE 'S' TO WRK-INCONSISTENTE
7C2511                 MOVE WRK-MSG43 TO OUT-MENSAG-01TM
7C2511                 GO TO 2213-99-FIM
7C2511             END-IF

7C2511*            VALIDA SE O CODIGO INFORMADO AO PROGRAMA É IGUAL AO CALCULADO
7C2511             IF WRK-CAD-CONTROLE NOT EQUAL INP-CNPJCTR-01TM-N
7C2511                 MOVE WRK-49353 TO WRK-OUT-ATTR-CNPJCTR-01TM
7C2511                 MOVE 'S' TO WRK-INCONSISTENTE
7C2511                 MOVE WRK-MSG43 TO OUT-MENSAG-01TM
7C2511                 GO TO 2213-99-FIM
7C2511             END-IF
7C2511         END-IF.

           IF INP-INSCRES-01TM NUMERIC OR
              INP-INSCRES-01TM EQUAL SPACES OR LOW-VALUES
              NEXT SENTENCE
           ELSE
              MOVE    WRK-49353
              TO      OUT-ATTR-INSCRES-01TM
              MOVE    'S'              TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-RAMO-01TM       NOT NUMERIC OR
                   INP-RAMO-01TM       EQUAL       ZEROS
                   MOVE    OUT-RAMO-01TM     TO    INP-RAMO-01TM
                   MOVE    WRK-49353      TO          OUT-ATTR-RAMO-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

7C2511*    IF INP-CNPJFIL-01TM-N      NOT EQUAL       ZEROS
           IF INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
              INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
              IF   INP-NMFANT-01TM     EQUAL       SPACES      OR
                   INP-NMFANT-01TM     EQUAL       LOW-VALUES
                   MOVE    WRK-49353
                   TO          OUT-ATTR-NMFANT-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF
           END-IF.

           IF      INP-NMPLAQ-01TM     EQUAL       SPACES      OR
                   INP-NMPLAQ-01TM     EQUAL       LOW-VALUES
                   MOVE    WRK-49353
                   TO          OUT-ATTR-NMPLAQ-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-CEPNRO2-01TM    NOT NUMERIC
                   MOVE    OUT-CEPNRO2-01TM  TO    INP-CEPNRO2-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CEPNRO2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-CEPNRO2-01TM    EQUAL ZEROS
                   MOVE    OUT-CEPNRO2-01TM  TO    INP-CEPNRO2-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CEPNRO2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-CEPSUF2-01TM    NOT NUMERIC
                   MOVE    OUT-CEPSUF2-01TM  TO    INP-CEPSUF2-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CEPNRO2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

1214       IF  INP-ENDER2-01TM         EQUAL SPACES OR LOW-VALUES
1214           MOVE WRK-49353          TO OUT-ATTR-ENDER2-01TM
1214           MOVE 'S'                TO WRK-INCONSISTENTE
1214       END-IF.

           IF      INP-NUMERO2-01TM    EQUAL       SPACES      OR
                   INP-NUMERO2-01TM    EQUAL       LOW-VALUES
                   MOVE    OUT-NUMERO2-01TM  TO    INP-NUMERO2-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-NUMERO2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

1214       MOVE INP-NUMERO2-01TM       TO WRK-NUMERO-AUX
1214       INSPECT WRK-NUMERO-AUX      REPLACING ALL ' ' BY '0'.

1214       IF  WRK-NUMERO-AUX-N        EQUAL ZEROS
1214           MOVE WRK-49353          TO OUT-ATTR-NUMERO2-01TM
1214           MOVE 'S'                TO WRK-INCONSISTENTE
1214                                      WRK-ERR-NUM-ENDERECO
1214       END-IF.

1214       IF  WRK-NUMERO-AUX          NOT NUMERIC
1214           IF  WRK-NUMERO-AUX      NOT EQUAL 'S/N00'
1214               MOVE WRK-49353      TO OUT-ATTR-NUMERO2-01TM
1214               MOVE 'S'            TO WRK-INCONSISTENTE
1214                                      WRK-ERR-NUM-ENDERECO
1214           END-IF
1214       END-IF.

           IF      INP-CIDADE2-01TM    EQUAL       SPACES      OR
                   INP-CIDADE2-01TM    EQUAL       LOW-VALUES
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CIDADE2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-UF2-01TM        EQUAL       SPACES      OR
                   INP-UF2-01TM        EQUAL       LOW-VALUES
                   MOVE    WRK-49353      TO          OUT-ATTR-UF2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-TDDD1-01TM      NOT NUMERIC OR
                   INP-TDDD1-01TM      EQUAL ZEROS
                   MOVE    OUT-TDDD1-01TM    TO    INP-TDDD1-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-TDDD1-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-TFONE1-01TM     NOT NUMERIC OR
                   INP-TFONE1-01TM     EQUAL ZEROS
                   MOVE    OUT-TFONE1-01TM    TO   INP-TFONE1-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-TFONE1-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-FDDD1-01TM      NOT NUMERIC
                   MOVE    OUT-FDDD1-01TM    TO    INP-FDDD1-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-FDDD1-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-FFONE1-01TM     NOT NUMERIC
                   MOVE    OUT-FFONE1-01TM    TO   INP-FFONE1-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-FFONE1-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-CONTR1-01TM     NOT EQUAL  SPACES AND LOW-VALUES
              IF   INP-CONTR1-01TM     NOT NUMERIC
                   MOVE    OUT-CONTR1-01TM    TO   INP-CONTR1-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CONTR1-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF
           END-IF.

           IF      INP-CONTAT1-01TM    EQUAL   SPACES
                   MOVE    OUT-CONTAT1-01TM   TO   INP-CONTAT1-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CONTAT1-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-CEPNRO3-01TM    NOT NUMERIC
                   MOVE    OUT-CEPNRO3-01TM  TO    INP-CEPNRO3-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CEPNRO3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-CEPNRO3-01TM    EQUAL ZEROS
                   MOVE    OUT-CEPNRO3-01TM  TO    INP-CEPNRO3-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CEPNRO3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-CEPSUF3-01TM    NOT NUMERIC
                   MOVE    OUT-CEPSUF3-01TM  TO    INP-CEPSUF3-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CEPSUF3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

1214       IF  INP-ENDER3-01TM         EQUAL SPACES OR LOW-VALUES
1214           MOVE WRK-49353          TO OUT-ATTR-ENDER3-01TM
1214           MOVE 'S'                TO WRK-INCONSISTENTE
1214       END-IF.

           IF      INP-NUMERO3-01TM    EQUAL       SPACES      OR
                   INP-NUMERO3-01TM    EQUAL       LOW-VALUES
                   MOVE    WRK-49353
                   TO          OUT-ATTR-NUMERO3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

1214       MOVE INP-NUMERO3-01TM       TO WRK-NUMERO-AUX
1214       INSPECT WRK-NUMERO-AUX      REPLACING ALL ' ' BY '0'.

1214       IF  WRK-NUMERO-AUX-N        EQUAL ZEROS
1214           MOVE WRK-49353          TO OUT-ATTR-NUMERO3-01TM
1214           MOVE 'S'                TO WRK-INCONSISTENTE
1214                                      WRK-ERR-NUM-ENDERECO
1214       END-IF.

           IF  WRK-NUMERO-AUX          NOT NUMERIC
1214           IF  WRK-NUMERO-AUX      NOT EQUAL 'S/N00'
1214               MOVE WRK-49353      TO OUT-ATTR-NUMERO3-01TM
1214               MOVE 'S'            TO WRK-INCONSISTENTE
1214                                      WRK-ERR-NUM-ENDERECO
1214           END-IF
1214       END-IF.

           IF      INP-CIDADE3-01TM    EQUAL       SPACES      OR
                   INP-CIDADE3-01TM    EQUAL       LOW-VALUES
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CIDADE3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-UF3-01TM        EQUAL       SPACES      OR
                   INP-UF3-01TM        EQUAL       LOW-VALUES
                   MOVE    WRK-49353      TO          OUT-ATTR-UF3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF INP-PROPR1-01TM     EQUAL       SPACES      OR
              INP-PROPR1-01TM     EQUAL       LOW-VALUES
              MOVE    WRK-49353      TO          OUT-ATTR-PROPR1-01TM
              MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF

           IF INP-CPFP1-01TM      NOT NUMERIC
              MOVE    OUT-CPFP1-01TM  TO      INP-CPFP1-01TM
              MOVE    WRK-49353      TO          OUT-ATTR-CPFP1-01TM
              MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF

           IF INP-DIGP1-01TM      NOT NUMERIC
              MOVE    OUT-DIGP1-01TM  TO      INP-DIGP1-01TM
              MOVE    WRK-49353      TO          OUT-ATTR-CPFP1-01TM
              MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF

           IF INP-DDNASC1-01TM    NOT NUMERIC
              MOVE    OUT-DDNASC1-01TM  TO    INP-DDNASC1-01TM
              MOVE    WRK-49353      TO          OUT-ATTR-DDNASC1-01TM
              MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF

           IF INP-MMNASC1-01TM    NOT NUMERIC
              MOVE    OUT-MMNASC1-01TM  TO    INP-MMNASC1-01TM
              MOVE    WRK-49353      TO          OUT-ATTR-MMNASC1-01TM
              MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF

           IF INP-AANASC1-01TM    NOT NUMERIC
               MOVE    OUT-AANASC1-01TM  TO    INP-AANASC1-01TM
               MOVE    WRK-49353      TO          OUT-ATTR-AANASC1-01TM
               MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF

7C2511*    IF INP-CNPJFIL-01TM-N      NOT EQUAL       ZEROS
           IF INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
              INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
              IF INP-CPFP1-01TM       IS  NUMERIC     AND
                 INP-PROPR1-01TM      EQUAL           SPACES
                 MOVE    WRK-49353     TO          OUT-ATTR-PROPR2-01TM
                 MOVE    WRK-49353     TO          OUT-ATTR-CPFP2-01TM
                 MOVE    WRK-49353     TO          OUT-ATTR-DDNASC2-01TM
                                                   OUT-ATTR-MMNASC2-01TM
                                                   OUT-ATTR-AANASC2-01TM
                 MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF
           END-IF.

7C2511*    IF INP-CNPJFIL-01TM-N       NOT EQUAL       ZEROS
           IF INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
              INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
              IF ( INP-PROPR2-01TM     NOT EQUAL       SPACES )    AND
                 ( INP-DDNASC2-01TM    EQUAL           SPACES      OR
                   INP-MMNASC2-01TM    EQUAL           SPACES      OR
                   INP-AANASC2-01TM    EQUAL           SPACES      OR
                   INP-CPFP2-01TM      EQUAL           SPACES      OR
                   INP-DIGP2-01TM      EQUAL           SPACES )
                   MOVE WRK-49353      TO          OUT-ATTR-PROPR2-01TM
                   MOVE WRK-49353      TO          OUT-ATTR-CPFP2-01TM
                   MOVE WRK-49353      TO          OUT-ATTR-DDNASC2-01TM
                                                   OUT-ATTR-MMNASC2-01TM
                                                   OUT-ATTR-AANASC2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF

              IF ( INP-CPFP2-01TM      NOT EQUAL       SPACES      OR
                   INP-DIGP2-01TM      NOT EQUAL       SPACES )    AND
                 ( INP-DDNASC2-01TM    EQUAL           SPACES      OR
                   INP-MMNASC2-01TM    EQUAL           SPACES      OR
                   INP-AANASC2-01TM    EQUAL           SPACES      OR
                   INP-PROPR2-01TM     EQUAL           SPACES )
                   MOVE WRK-49353      TO          OUT-ATTR-PROPR2-01TM
                                                   OUT-ATTR-CPFP2-01TM
                                                   OUT-ATTR-DIGP2-01TM
                                                   OUT-ATTR-DDNASC2-01TM
                                                   OUT-ATTR-MMNASC2-01TM
                                                   OUT-ATTR-AANASC2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF

              IF ( INP-DDNASC2-01TM    NOT EQUAL       SPACES      OR
                   INP-MMNASC2-01TM    NOT EQUAL       SPACES      OR
                   INP-AANASC2-01TM    NOT EQUAL       SPACES )    AND
                 ( INP-PROPR2-01TM     EQUAL           SPACES      OR
                   INP-CPFP2-01TM      EQUAL           SPACES      OR
                   INP-DIGP2-01TM      EQUAL           SPACES )
                   MOVE WRK-49353      TO          OUT-ATTR-PROPR2-01TM
                                                   OUT-ATTR-CPFP2-01TM
                                                   OUT-ATTR-DIGP2-01TM
                                                   OUT-ATTR-DDNASC2-01TM
                                                   OUT-ATTR-MMNASC2-01TM
                                                   OUT-ATTR-AANASC2-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF
           END-IF.

7C2511*    IF INP-CNPJFIL-01TM-N      NOT EQUAL       ZEROS
           IF INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
              INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
              IF INP-CPFP3-01TM      IS  NUMERIC     AND
                 INP-PROPR3-01TM     EQUAL           SPACES
                   MOVE WRK-49353      TO          OUT-ATTR-PROPR3-01TM
                                                   OUT-ATTR-CPFP3-01TM
                                                   OUT-ATTR-DIGP3-01TM
                                                   OUT-ATTR-DDNASC3-01TM
                                                   OUT-ATTR-MMNASC3-01TM
                                                   OUT-ATTR-AANASC3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF
           END-IF.

7C2511*    IF INP-CNPJFIL-01TM-N      NOT EQUAL       ZEROS
           IF INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
              INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
              IF ( INP-PROPR3-01TM     NOT EQUAL       SPACES )    AND
                 ( INP-DDNASC3-01TM    EQUAL           SPACES      OR
                   INP-MMNASC3-01TM    EQUAL           SPACES      OR
                   INP-AANASC3-01TM    EQUAL           SPACES      OR
                   INP-CPFP3-01TM      EQUAL           SPACES      OR
                   INP-DIGP3-01TM      EQUAL           SPACES )
                   MOVE WRK-49353      TO          OUT-ATTR-PROPR3-01TM
                                                   OUT-ATTR-CPFP3-01TM
                                                   OUT-ATTR-DIGP3-01TM
                                                   OUT-ATTR-DDNASC3-01TM
                                                   OUT-ATTR-MMNASC3-01TM
                                                   OUT-ATTR-AANASC3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF

              IF ( INP-CPFP3-01TM      NOT EQUAL       SPACES      OR
                   INP-DIGP3-01TM      NOT EQUAL       SPACES )    AND
                 ( INP-DDNASC3-01TM    EQUAL           SPACES      OR
                   INP-MMNASC3-01TM    EQUAL           SPACES      OR
                   INP-AANASC3-01TM    EQUAL           SPACES      OR
                   INP-PROPR3-01TM     EQUAL           SPACES )
                   MOVE WRK-49353      TO          OUT-ATTR-PROPR3-01TM
                                                   OUT-ATTR-CPFP3-01TM
                                                   OUT-ATTR-DIGP3-01TM
                                                   OUT-ATTR-DDNASC3-01TM
                                                   OUT-ATTR-MMNASC3-01TM
                                                   OUT-ATTR-AANASC3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF

              IF ( INP-DDNASC3-01TM    NOT EQUAL       SPACES      OR
                   INP-MMNASC3-01TM    NOT EQUAL       SPACES      OR
                   INP-AANASC3-01TM    NOT EQUAL       SPACES )    AND
                 ( INP-PROPR3-01TM     EQUAL           SPACES      OR
                   INP-CPFP3-01TM      EQUAL           SPACES      OR
                   INP-DIGP3-01TM      EQUAL           SPACES )
                   MOVE WRK-49353      TO          OUT-ATTR-PROPR3-01TM
                                                   OUT-ATTR-CPFP3-01TM
                                                   OUT-ATTR-DIGP3-01TM
                                                   OUT-ATTR-DDNASC3-01TM
                                                   OUT-ATTR-MMNASC3-01TM
                                                   OUT-ATTR-AANASC3-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
              END-IF
           END-IF.

           IF      INP-BANCO-01TM      NOT NUMERIC
                   MOVE    OUT-BANCO-01TM    TO    INP-BANCO-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-BANCO-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-BANCO-01TM      EQUAL  ZEROS
                   MOVE    OUT-BANCO-01TM    TO    INP-BANCO-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-BANCO-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-AGENCIA-01TM    NOT NUMERIC
                   MOVE    OUT-AGENCIA-01TM  TO    INP-AGENCIA-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-AGENCIA-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           ELSE
              IF      INP-AGENCIA-01TM    EQUAL ZEROS
                      MOVE    OUT-AGENCIA-01TM  TO INP-AGENCIA-01TM
                      MOVE    WRK-49353
                      TO       OUT-ATTR-AGENCIA-01TM
                      MOVE    'S'         TO       WRK-INCONSISTENTE
              ELSE
                 IF    INP-AGENCIA-01TM-N  EQUAL  WRK-COD-DEPTO-N
                       MOVE   WRK-225     TO      OUT-ATTR-AGENCIA-01TM
                 ELSE
                       NEXT    SENTENCE
                 END-IF
              END-IF
           END-IF.

           IF      INP-CONTA-01TM      NOT NUMERIC OR
MARC               INP-CONTA-01TM      EQUAL ZEROS
                   MOVE    OUT-CONTA-01TM    TO    INP-CONTA-01TM
                   MOVE    WRK-49353
                   TO          OUT-ATTR-CONTA-01TM
                   MOVE    'S'         TO          WRK-INCONSISTENTE
           END-IF.

           IF      INP-DIGCTA-01TM     NOT NUMERIC
MARC           IF  INP-DIGCTA-01TM     NOT EQUAL 'P'
MARC               MOVE    OUT-DIGCTA-01TM    TO   INP-DIGCTA-01TM
MARC               MOVE    WRK-49353
                   TO   OUT-ATTR-DIGCTA-01TM
MARC               MOVE    'S'                TO   WRK-INCONSISTENTE
MARC           END-IF
           END-IF.

       2213-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2221-CONSISTENCIA-FISICA2       SECTION.
      *----------------------------------------------------------------*

7C2511*    IF  INP-CNPJFIL-01TM-N      NOT EQUAL   ZEROS
           IF  INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
               INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
7C2511*        MOVE    INP-CPFP1-01TM-N        TO  WRK-CPF-CGC
               MOVE    INP-CPFP1-01TM-X        TO  WRK-CPF-CGC
               MOVE    ZEROS                   TO  WRK-FILIAL

               PERFORM 2222-CONSISTIR-CPF

               IF  WRK-INCONSISTENTE       NOT EQUAL       SPACES
                   MOVE    WRK-49353
                   TO      OUT-ATTR-CPFP1-01TM
                   MOVE    WRK-MSG20       TO      OUT-MENSAG-01TM
                   GO      TO              2221-99-FIM
               END-IF

               IF  INP-DIGP1-01TM          NOT EQUAL       WRK-CONTROLE
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353
                   TO      OUT-ATTR-CPFP1-01TM
                   MOVE    WRK-MSG14       TO      OUT-MENSAG-01TM
                   GO      TO              2221-99-FIM
               END-IF
           END-IF

7C2511*    IF  INP-CNPJFIL-01TM-N      NOT EQUAL   ZEROS
           IF  INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
               INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
               IF  INP-PROPR2-01TM     NOT EQUAL       SPACES       AND
                   INP-PROPR2-01TM     NOT EQUAL       LOW-VALUES
7C2511*            MOVE    INP-CPFP2-01TM-N        TO      WRK-CPF-CGC
                   MOVE    INP-CPFP2-01TM-X        TO      WRK-CPF-CGC
                   MOVE    ZEROS                   TO      WRK-FILIAL
                   PERFORM         2222-CONSISTIR-CPF
                   IF      WRK-INCONSISTENTE   NOT EQUAL   SPACES
                           MOVE    WRK-MSG20   TO      OUT-MENSAG-01TM
                           MOVE    WRK-49353
                           TO   OUT-ATTR-CPFP2-01TM
                           GO      TO          2221-99-FIM
                   END-IF
                   IF      INP-DIGP2-01TM      NOT EQUAL   WRK-CONTROLE
                           MOVE    'S'         TO      WRK-INCONSISTENTE
                           MOVE    WRK-49353
                           TO  OUT-ATTR-CPFP2-01TM
                           MOVE    WRK-MSG14   TO      OUT-MENSAG-01TM
                           GO      TO          2221-99-FIM
                   END-IF
               END-IF
           END-IF.

7C2511*    IF  INP-CNPJFIL-01TM-N      NOT EQUAL   ZEROS
           IF  INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
               INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
               IF  INP-PROPR3-01TM     NOT EQUAL       SPACES       AND
                   INP-PROPR3-01TM     NOT EQUAL       LOW-VALUES
7C2511*            MOVE    INP-CPFP3-01TM-N        TO      WRK-CPF-CGC
                   MOVE    INP-CPFP3-01TM-X        TO      WRK-CPF-CGC
                   MOVE    ZEROS                   TO      WRK-FILIAL
                   PERFORM         2222-CONSISTIR-CPF
                   IF      WRK-INCONSISTENTE   NOT EQUAL   SPACES
                           MOVE    WRK-MSG20   TO      OUT-MENSAG-01TM
                           MOVE    WRK-49353
                           TO    OUT-ATTR-CPFP3-01TM
                           GO      TO          2221-99-FIM
                   END-IF
                   IF      INP-DIGP3-01TM      NOT EQUAL   WRK-CONTROLE
                           MOVE    'S'         TO      WRK-INCONSISTENTE
                           MOVE    WRK-49353
                           TO    OUT-ATTR-CPFP3-01TM
                           MOVE    WRK-MSG14   TO      OUT-MENSAG-01TM
                           GO      TO          2221-99-FIM
                   END-IF
               END-IF
           END-IF.

           IF INP-CNPJFIL-01TM-N      NOT EQUAL   ZEROS
              IF INP-CPFP1-01TM       NOT EQUAL   ZEROS AND SPACES
                 IF INP-CPFP1-01TM    EQUAL INP-CPFP2-01TM
                    MOVE    'S'        TO      WRK-INCONSISTENTE
                    MOVE    WRK-49353
                    TO          OUT-ATTR-CPFP1-01TM
                    MOVE    WRK-49353
                    TO          OUT-ATTR-CPFP2-01TM
                    MOVE    WRK-MSG19  TO      OUT-MENSAG-01TM
                    GO      TO         2221-99-FIM
                 ELSE
                    IF INP-CPFP1-01TM    EQUAL INP-CPFP3-01TM
                       MOVE    'S'        TO      WRK-INCONSISTENTE
                       MOVE    WRK-49353
                       TO         OUT-ATTR-CPFP1-01TM
                       MOVE    WRK-49353
                       TO         OUT-ATTR-CPFP3-01TM
                       MOVE    WRK-MSG19  TO      OUT-MENSAG-01TM
                       GO      TO         2221-99-FIM
                    END-IF
                 END-IF
              END-IF
           END-IF.

           IF INP-CNPJFIL-01TM-N      NOT EQUAL   ZEROS
              IF INP-CPFP2-01TM       NOT EQUAL   ZEROS AND SPACES
                 IF INP-CPFP2-01TM    EQUAL INP-CPFP3-01TM
                    MOVE    'S'        TO      WRK-INCONSISTENTE
                    MOVE    WRK-49353
                    TO          OUT-ATTR-CPFP2-01TM
                    MOVE    WRK-49353
                    TO          OUT-ATTR-CPFP3-01TM
                    MOVE    WRK-MSG19  TO      OUT-MENSAG-01TM
                    GO      TO         2221-99-FIM
                 END-IF
              END-IF
           END-IF.

           MOVE    INP-DDNASC1-01TM-N      TO  WRK-DIA-AAAAMMDD
           MOVE    INP-MMNASC1-01TM-N      TO  WRK-MES-AAAAMMDD
           MOVE    INP-AANASC1-01TM-N      TO  WRK-ANO-AAAAMMDD

           PERFORM 2223-CONSISTIR-DATA-NASC

           IF  WRK-INCONSISTENTE       NOT EQUAL       SPACES
               MOVE    WRK-49353
               TO          OUT-ATTR-DDNASC1-01TM
               GO      TO              2221-99-FIM
           END-IF.

           IF  WRK-OPCAO               EQUAL       'I'
               MOVE    'S'             TO      WRK-INCONSISTENTE
               MOVE    WRK-49353       TO      OUT-ATTR-DDNASC1-01TM
                                               OUT-ATTR-MMNASC1-01TM
                                               OUT-ATTR-AANASC1-01TM
               MOVE    WRK-MSG13       TO      OUT-MENSAG-01TM
               GO      TO              2221-99-FIM
           END-IF.

7C2511*    IF  INP-CNPJFIL-01TM-N      NOT EQUAL   ZEROS
           IF  INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
               INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
               IF  INP-DDNASC2-01TM      NOT EQUAL     SPACES       AND
                   INP-MMNASC2-01TM      NOT EQUAL     SPACES       AND
                   INP-AANASC2-01TM      NOT EQUAL     SPACES
                   MOVE    INP-DDNASC2-01TM-N    TO    WRK-DIA-AAAAMMDD
                   MOVE    INP-MMNASC2-01TM-N    TO    WRK-MES-AAAAMMDD
                   MOVE    INP-AANASC2-01TM-N    TO    WRK-ANO-AAAAMMDD
                   PERFORM         2223-CONSISTIR-DATA-NASC
                   IF      WRK-INCONSISTENTE   NOT EQUAL   SPACES
                           MOVE    WRK-49353
                           TO  OUT-ATTR-DDNASC2-01TM
                           GO      TO          2221-99-FIM
                   END-IF
                   IF      WRK-OPCAO           EQUAL       'I'
                           MOVE    'S'         TO      WRK-INCONSISTENTE
                           MOVE    WRK-49353
                           TO  OUT-ATTR-DDNASC2-01TM
                               OUT-ATTR-MMNASC2-01TM
                               OUT-ATTR-AANASC2-01TM

                           MOVE    WRK-MSG13   TO      OUT-MENSAG-01TM
                           GO      TO          2221-99-FIM
                   END-IF
               END-IF
           END-IF.

7C2511*    IF  INP-CNPJFIL-01TM-N      NOT EQUAL   ZEROS
           IF  INP-CNPJFIL-01TM-X      NOT EQUAL   SPACES AND
               INP-CNPJFIL-01TM-X      NOT EQUAL   LOW-VALUES
               IF  INP-DDNASC3-01TM      NOT EQUAL     SPACES       AND
                   INP-MMNASC3-01TM      NOT EQUAL     SPACES       AND
                   INP-AANASC3-01TM      NOT EQUAL     SPACES
                   MOVE    INP-DDNASC3-01TM-N    TO    WRK-DIA-AAAAMMDD
                   MOVE    INP-MMNASC3-01TM-N    TO    WRK-MES-AAAAMMDD
                   MOVE    INP-AANASC3-01TM-N    TO    WRK-ANO-AAAAMMDD
                   PERFORM         2223-CONSISTIR-DATA-NASC
                   IF      WRK-INCONSISTENTE   NOT EQUAL   SPACES
                           MOVE    WRK-49353
                           TO  OUT-ATTR-DDNASC3-01TM
                               OUT-ATTR-MMNASC3-01TM
                               OUT-ATTR-AANASC3-01TM

                           GO      TO          2221-99-FIM
                   END-IF
                   IF      WRK-OPCAO           EQUAL       'I'
                           MOVE    'S'         TO      WRK-INCONSISTENTE
                           MOVE    WRK-49353
                           TO  OUT-ATTR-DDNASC3-01TM
                               OUT-ATTR-MMNASC3-01TM
                               OUT-ATTR-AANASC3-01TM
                           MOVE    WRK-MSG13   TO      OUT-MENSAG-01TM
                           GO      TO          2221-99-FIM
                   END-IF
               END-IF
           END-IF.

           MOVE    INP-CONTA-01TM-N        TO      WRK-CAMPO-DIG.
           MOVE    07                      TO      WRK-TAMANHO-DIG.

           CALL    'POOL0431'          USING       WRK-CAMPO-DIG
                                                   WRK-DIGITO-DIG
                                                   WRK-TAMANHO-DIG.

           IF      RETURN-CODE             NOT EQUAL       ZEROS
                   MOVE    RETURN-CODE     TO      WRK-MSG10-RET-CODE
                   MOVE    'POOL0431'      TO      WRK-MSG10-NM-POOL
                   MOVE    WRK-MSG10       TO      OUT-MENSAG-01TM
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   GO      TO          2221-99-FIM
           END-IF.

           IF      WRK-DIGITO-DIG      EQUAL       '.'
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353
                   TO      OUT-ATTR-DIGCTA-01TM
                   MOVE    WRK-MSG18       TO      OUT-MENSAG-01TM
                   GO      TO          2221-99-FIM
           END-IF.

           IF      WRK-DIGITO-DIG   NOT EQUAL      INP-DIGCTA-01TM-N
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353
                   TO      OUT-ATTR-DIGCTA-01TM
                   MOVE    WRK-MSG18       TO      OUT-MENSAG-01TM
                   GO      TO          2221-99-FIM
           END-IF.

       2221-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2222-CONSISTIR-CPF              SECTION.
      *----------------------------------------------------------------*

7C2511*    CHAMADA DO MODULO BRAD2000
           MOVE WRK-CPF-CGC      TO WRK-CAD-NUMERO.
           MOVE WRK-FILIAL       TO WRK-CAD-FILIAL.
           CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
                                       WRK-CAD-CONTROLE.

           IF      RETURN-CODE             NOT EQUAL       ZEROS
                   MOVE    'S'             TO      WRK-INCONSISTENTE
           END-IF.

       2222-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2223-CONSISTIR-DATA-NASC        SECTION.
      *----------------------------------------------------------------*

      *-------------------------- FORMATO      ( AAAAMMDD )
           MOVE    WRK-DATA-AAAAMMDD-R     TO      WRK-DATA-ENVIADA
           MOVE    SPACES                  TO      WRK-OPCAO

           CALL    'POOL1205'          USING       WRK-AREA-POOL1205
                                                   WRK-MENSA-POOL1205.

           IF      RETURN-CODE             NOT EQUAL       ZEROS
                   MOVE    RETURN-CODE     TO      WRK-MSG10-RET-CODE
                   MOVE    'POOL1205'      TO      WRK-MSG10-NM-POOL
                   MOVE    WRK-MSG13       TO      OUT-MENSAG-01TM
                   MOVE    'S'             TO      WRK-INCONSISTENTE
           END-IF.

       2223-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2231-CONSISTENCIA-LOGICA        SECTION.
      *----------------------------------------------------------------*

           MOVE    INP-RAMO-01TM-N         TO          WRK-E-COD-RAMO.

           PERFORM         1230-ACESSAR-BVVE7010.

           IF      BVVE21-COD-RETORNO      EQUAL      03
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353          TO      OUT-ATTR-RAMO-01TM
                   MOVE    WRK-MSG02       TO      OUT-MENSAG-01TM
                   GO      TO              2231-99-FIM
           ELSE
                   MOVE    WRK-S-DESC-RAMO TO      INP-IRAMO-01TM
           END-IF.

           MOVE    INP-CEPNRO2-01TM-N      TO      WRK-E-CEP
           MOVE    INP-CEPSUF2-01TM-N      TO      WRK-E-COMPL-CEP

           PERFORM         2232-VALIDAR-CEP.

           IF      BVVE21-COD-RETORNO      EQUAL           03
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353
                   TO      OUT-ATTR-CEPNRO2-01TM
                   MOVE    WRK-49353
                   TO      OUT-ATTR-CEPSUF2-01TM
                   MOVE    WRK-MSG12       TO      OUT-MENSAG-01TM
                   GO      TO              2231-99-FIM
           END-IF.

           MOVE    INP-CEPNRO3-01TM-N      TO      WRK-E-CEP
           MOVE    INP-CEPSUF3-01TM-N      TO      WRK-E-COMPL-CEP

           PERFORM         2232-VALIDAR-CEP.

           IF      BVVE21-COD-RETORNO      EQUAL           03
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353
                   TO      OUT-ATTR-CEPNRO3-01TM
                   MOVE    WRK-49353
                   TO      OUT-ATTR-CEPSUF3-01TM
                   MOVE    WRK-MSG12       TO      OUT-MENSAG-01TM
                   GO      TO              2231-99-FIM
           END-IF.

           PERFORM         2233-VALIDAR-BANCO.

           IF      WRK-INCONSISTENTE       NOT EQUAL       SPACES
                   GO      TO              2231-99-FIM
           END-IF.

           PERFORM         2234-VALIDAR-AGENCIA.

           IF      WRK-INCONSISTENTE       NOT EQUAL       SPACE
                   GO      TO              2231-99-FIM
           END-IF.

           MOVE    INP-BANCO-01TM-N    TO  WRK-E-BANCO-CLIE
           MOVE    INP-AGENCIA-01TM-N  TO  WRK-E-AGENCIA-CLIE
           MOVE    INP-CONTA-01TM-N    TO  WRK-E-CONTA-CLIE
           MOVE    INP-DIGCTA-01TM-N   TO  WRK-E-DIG-CONTA-CLIE
           PERFORM 1220-ACESSAR-BVVE7008.

           IF      BVVE21-COD-RETORNO          EQUAL      02
                   MOVE    'S'            TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353      TO      OUT-ATTR-BANCO-01TM
                   MOVE    WRK-49353      TO      OUT-ATTR-AGENCIA-01TM
                   MOVE    WRK-49353      TO      OUT-ATTR-CONTA-01TM
                   MOVE    WRK-49353      TO      OUT-ATTR-DIGCTA-01TM
                   MOVE    ERR-TEXTO      TO      OUT-MENSAG-01TM
                   GO      TO          2231-99-FIM
           END-IF.

           IF      BVVE21-COD-RETORNO          EQUAL      01  AND
                   ERR-TIPO-ACESSO             EQUAL    'APL'
                   MOVE    'S'         TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353   TO      OUT-ATTR-AGENCIA-01TM
                   MOVE    ERR-TEXTO   TO      OUT-MENSAG-01TM
                   GO      TO          2231-99-FIM
           END-IF.

MARC       IF      BVVE21-COD-RETORNO          EQUAL      03
MARC               MOVE    'S'         TO      WRK-INCONSISTENTE
MARC               MOVE    WRK-49353      TO      OUT-ATTR-AGENCIA-01TM
MARC               MOVE    WRK-49353      TO      OUT-ATTR-CONTA-01TM
MARC               MOVE    WRK-49353      TO      OUT-ATTR-DIGCTA-01TM
MARC               MOVE    WRK-MSG17   TO      OUT-MENSAG-01TM
MARC               GO      TO          2231-99-FIM
MARC       END-IF.

           IF      WRK-INCONSISTENTE       NOT EQUAL       SPACE
                   GO      TO              2231-99-FIM
           END-IF.

       2231-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2232-VALIDAR-CEP                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVE21-AREA-ENTRADA
                       BVVE22-AREA-VARIAVEIS.

           MOVE    'OPRD'                  TO      BVVE21-COMANDO.
           MOVE    'BVVE7007'              TO      BVVE21-MODULO.
           MOVE    WRK-ENVIA-DADOS-CEP     TO      BVVE21-PARAMETROS.
           MOVE    130                     TO      BVVE21-TAM-REGISTRO.

           MOVE    'BVVE7920'              TO      WRK-MODULO.

           CALL    WRK-MODULO      USING     BVVE21-AREA-ENTRADA
                                             BVVE22-AREA-VARIAVEIS
                                             ERRO-AREA
                                             WRK-SQLCA
                                             WRK-RECEBE-DADOS-CEP.

       2232-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2233-VALIDAR-BANCO              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVE21-AREA-ENTRADA
                       BVVE22-AREA-VARIAVEIS.

           MOVE    INP-BANCO-01TM-N        TO      WRK-E-BANCO

           MOVE    'OPRD'                  TO      BVVE21-COMANDO.
           MOVE    'BVVE7005'              TO      BVVE21-MODULO.
           MOVE    WRK-ENVIA-DADOS-BANCO   TO      BVVE21-PARAMETROS.
           MOVE    043                     TO      BVVE21-TAM-REGISTRO.

           MOVE    'BVVE7920'              TO      WRK-MODULO.

           CALL    WRK-MODULO      USING     BVVE21-AREA-ENTRADA
                                             BVVE22-AREA-VARIAVEIS
                                             ERRO-AREA
                                             WRK-SQLCA
                                             WRK-RECEBE-DADOS-BANCO.

           IF      BVVE21-COD-RETORNO      EQUAL           03
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353
                   TO      OUT-ATTR-BANCO-01TM
                   MOVE    WRK-MSG15       TO      OUT-MENSAG-01TM
           END-IF.

       2233-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2234-VALIDAR-AGENCIA            SECTION.
      *----------------------------------------------------------------*


           INITIALIZE  BVVE21-AREA-ENTRADA
                       BVVE22-AREA-VARIAVEIS.

           MOVE    INP-BANCO-01TM-N        TO      WRK-E-BCO-AGENCIA.
           MOVE    INP-AGENCIA-01TM-N      TO      WRK-E-AGE-AGENCIA.

           MOVE    'OPRD'                  TO      BVVE21-COMANDO.
           MOVE    'BVVE7006'              TO      BVVE21-MODULO.
           MOVE    WRK-ENVIA-DADOS-AGENC   TO      BVVE21-PARAMETROS.
           MOVE    048                     TO      BVVE21-TAM-REGISTRO.

           MOVE    'BVVE7920'              TO      WRK-MODULO.

           CALL    WRK-MODULO      USING     BVVE21-AREA-ENTRADA
                                             BVVE22-AREA-VARIAVEIS
                                             ERRO-AREA
                                             WRK-SQLCA
                                             WRK-RECEBE-DADOS-AGENC.

           IF      BVVE21-COD-RETORNO      NOT EQUAL       ZEROS    AND
                   BVVE21-COD-RETORNO      NOT EQUAL       03
                   MOVE BVVE21-MODULO      TO ERR-MODULO
                   PERFORM     9999-ROTINA-ERRO
           END-IF.

           IF      BVVE21-COD-RETORNO      EQUAL           01 OR
                   BVVE21-COD-RETORNO      EQUAL           03
                   MOVE    'S'             TO      WRK-INCONSISTENTE
                   MOVE    WRK-49353
                   TO      OUT-ATTR-AGENCIA-01TM
                   MOVE    WRK-MSG16       TO      OUT-MENSAG-01TM
                   GO      TO      2234-99-FIM
           END-IF.

       2234-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2241-PROTEGER-CAMPOS            SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-241              TO       OUT-ATTR-INSCRES-01TM
           MOVE   WRK-225              TO       OUT-ATTR-RAMO-01TM
                                                OUT-ATTR-NMFANT-01TM
                                                OUT-ATTR-NMPLAQ-01TM
                                                OUT-ATTR-CEPNRO1-01TM
                                                OUT-ATTR-CEPSUF1-01TM
                                                OUT-ATTR-ENDER1-01TM
                                                OUT-ATTR-NUMERO1-01TM
                                                OUT-ATTR-COMPL1-01TM
                                                OUT-ATTR-CIDADE1-01TM
                                                OUT-ATTR-UF1-01TM
                                                OUT-ATTR-CEPNRO2-01TM
                                                OUT-ATTR-CEPSUF2-01TM
                                                OUT-ATTR-ENDER2-01TM
                                                OUT-ATTR-NUMERO2-01TM
                                                OUT-ATTR-COMPL2-01TM
                                                OUT-ATTR-CIDADE2-01TM
                                                OUT-ATTR-UF2-01TM
                                                OUT-ATTR-TDDD1-01TM
                                                OUT-ATTR-TFONE1-01TM
                                                OUT-ATTR-FDDD1-01TM
                                                OUT-ATTR-FFONE1-01TM
                                                OUT-ATTR-CONTR1-01TM
                                                OUT-ATTR-CONTAT1-01TM
                                                OUT-ATTR-EMAIL1-01TM
                                                OUT-ATTR-CEPNRO3-01TM
                                                OUT-ATTR-CEPSUF3-01TM
                                                OUT-ATTR-ENDER3-01TM
                                                OUT-ATTR-NUMERO3-01TM
                                                OUT-ATTR-COMPL3-01TM
                                                OUT-ATTR-CIDADE3-01TM
                                                OUT-ATTR-UF3-01TM
                                                OUT-ATTR-PROPR1-01TM
                                                OUT-ATTR-CPFP1-01TM
                                                OUT-ATTR-DIGP1-01TM
                                                OUT-ATTR-DDNASC1-01TM
                                                OUT-ATTR-DDNASC1-01TM
                                                OUT-ATTR-MMNASC1-01TM
                                                OUT-ATTR-AANASC1-01TM
                                                OUT-ATTR-PROPR2-01TM
                                                OUT-ATTR-CPFP2-01TM
                                                OUT-ATTR-DIGP2-01TM
                                                OUT-ATTR-DDNASC2-01TM
                                                OUT-ATTR-DDNASC2-01TM
                                                OUT-ATTR-MMNASC2-01TM
                                                OUT-ATTR-AANASC2-01TM
                                                OUT-ATTR-PROPR3-01TM
                                                OUT-ATTR-CPFP3-01TM
                                                OUT-ATTR-DIGP3-01TM
                                                OUT-ATTR-DDNASC3-01TM
                                                OUT-ATTR-DDNASC3-01TM
                                                OUT-ATTR-MMNASC3-01TM
                                                OUT-ATTR-AANASC3-01TM
                                                OUT-ATTR-BANCO-01TM
                                                OUT-ATTR-AGENCIA-01TM
                                                OUT-ATTR-CONTA-01TM
                                                OUT-ATTR-DIGCTA-01TM.

       2241-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2300-TRATAR-PFK06            SECTION.
      *----------------------------------------------------------------*

JEM        IF  WRK-INP-INCALT-01TM EQUAL '*'
JEM            PERFORM 2302-CONSISTIR-SEJAEXIS
JEM            IF  WRK-VAL-INC-FIL     NOT EQUAL 'S'
JEM                GO TO 2300-99-FIM
JEM            END-IF
JEM        END-IF.

           PERFORM         2310-INCLUIR-PROPOSTA.

           IF      BVVE21-COD-RETORNO      NOT EQUAL   ZEROS
                   MOVE    WRK-MSG06       TO      OUT-MENSAG-01TM
                   PERFORM         1241-DESPROTEGER-CAMPOS
                   PERFORM         2400-DEVOLVE-TELA
                   GO      TO      2300-99-FIM
           END-IF.

           MOVE    WRK-MSG07           TO      BVVE25-MENSAGEM.
           MOVE ' '                    TO      WRK-INP-INCALT-01TM.

           MOVE 'BVVE2600'             TO      INP-CHAMADOR-01TM.
           PERFORM         2100-TRATAR-PFK03.

       2300-99-FIM.                 EXIT.

JEM   *----------------------------------------------------------------*
JEM    2301-TRATAR-PFK11            SECTION.
JEM   *----------------------------------------------------------------*
JEM
JEM        IF  WRK-INP-INCALT-01TM EQUAL '*'
JEM            PERFORM 2302-CONSISTIR-SEJAEXIS
JEM            IF  WRK-VAL-INC-FIL     NOT EQUAL 'S'
JEM                GO TO 2301-99-FIM
JEM            END-IF
JEM        END-IF.
JEM
JEM        PERFORM 2310-INCLUIR-PROPOSTA.
JEM
JEM        IF      BVVE21-COD-RETORNO      NOT EQUAL   ZEROS
JEM                MOVE    WRK-MSG06       TO      OUT-MENSAG-01TM
                   PERFORM         1241-DESPROTEGER-CAMPOS
                   PERFORM         2400-DEVOLVE-TELA
JEM                GO      TO      2301-99-FIM
JEM        END-IF.
JEM
JEM        MOVE '1'                    TO INP-FASE-01TM.
           PERFORM 1241-DESPROTEGER-CAMPOS
JEM
JEM        PERFORM 2400-DEVOLVE-TELA.
JEM
JEM        MOVE WRK-MSG41              TO OUT-MENSAG-01TM.
JEM        MOVE '*'                    TO WRK-INP-INCALT-01TM.
JEM
JEM   *----------------------------------------------------------------*
JEM    2301-99-FIM.                   EXIT.
JEM   *----------------------------------------------------------------*
JEM
JEM   *----------------------------------------------------------------*
JEM    2302-CONSISTIR-SEJAEXIS      SECTION.
JEM   *----------------------------------------------------------------*
JEM
JEM        MOVE 'S'                    TO WRK-VAL-INC-FIL.
JEM
JEM   *----------------------------------------------------------------*
JEM    2302-99-FIM.                   EXIT.
JEM   *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-INCLUIR-PROPOSTA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVE21-AREA-ENTRADA
                       BVVE22-AREA-VARIAVEIS.

7C2511*    IF INP-CNPJFIL-01TM NOT EQUAL ZEROS
           IF INP-CNPJFIL-01TM-X NOT EQUAL SPACES AND
              INP-CNPJFIL-01TM-X NOT EQUAL LOW-VALUES
              MOVE    INP-CNPJNRO-01TM-N    TO WRK-E-CCNPJ-CPF-ESTBL
              MOVE    INP-CNPJFIL-01TM-N    TO WRK-E-CFLIAL-CNPJ-ESTBL
              MOVE    INP-CNPJCTR-01TM-N    TO WRK-E-CCTRL-CNPJ-CPF
           ELSE
              MOVE    INP-CNPJNRO-01TM-N    TO WRK-E-CPF-NUMERO
              MOVE    INP-CNPJCTR-01TM-N    TO WRK-E-CPF-CONTR
              MOVE    SPACES                TO WRK-E-CPF-FILIAL
           END-IF.

           MOVE    INP-NMFANT-01TM       TO   WRK-E-IFANTS-ESTBL-AFLIA
           MOVE    INP-NOMEPRO-01TM      TO   WRK-E-IESTBL-AFLIA

7C2511*    IF INP-CNPJFIL-01TM NOT EQUAL ZEROS
           IF INP-CNPJFIL-01TM-X NOT EQUAL SPACES AND
              INP-CNPJFIL-01TM-X NOT EQUAL LOW-VALUES
              IF INP-INSCRES-01TM EQUAL SPACES OR LOW-VALUES
                 MOVE  ZEROS             TO   WRK-E-CINSCR-EST-ESTBL
              ELSE
                 MOVE  INP-INSCRES-01TM  TO   WRK-E-CINSCR-EST-ESTBL
              END-IF
           ELSE
              MOVE     INP-INSCRES-01TM  TO   WRK-E-CINSCR-EST-ESTBL
           END-IF.

           MOVE    INP-NMPLAQ-01TM       TO   WRK-E-IPLAQT-EQPMT-ESTBL
           MOVE    ZEROS                 TO   WRK-E-CSIT-PPSTA-AFLIA
           MOVE    SPACES                TO   WRK-E-RSIT-PPSTA-AFLIA
           MOVE    SPACES                TO   WRK-E-DENVIO-PPSTA-AFLIA
           MOVE    SPACES                TO   WRK-E-DRETOR-PPSTA-AFLIA
           MOVE    SPACES                TO   WRK-E-DCAD-PPSTA-AFLIA
           MOVE    INP-RAMO-01TM-N       TO   WRK-E-CTPO-ATVDD-COML
           MOVE    INP-IRAMO-01TM        TO   WRK-E-RATVDD-COML
           IF      INP-CONTR1-01TM   =   SPACES  OR  LOW-VALUES
                   MOVE  ZEROS                TO   WRK-E-CEMPR-CONTT
           ELSE
                   MOVE  INP-CONTR1-01TM-N    TO   WRK-E-CEMPR-CONTT
           END-IF
           MOVE    SPACES                TO   WRK-E-DATULZ-PPSTA-AFLIA
           MOVE    WRK-COD-USER          TO   WRK-E-CFUNC-BDSCO
           MOVE    INP-BANCO-01TM-N      TO   WRK-E-CEMPR-INC
           MOVE    INP-AGENCIA-01TM-N    TO   WRK-E-CJUNC-DEPDC-BDSCO

      *------------- SEGUNDO  ENDERECO -------
           MOVE    INP-CEPNRO2-01TM-N    TO   WRK-E-CCEP-1
           MOVE    INP-CEPSUF2-01TM-N    TO   WRK-E-CCEP-COMPL-1
           MOVE    INP-CIDADE2-01TM      TO   WRK-E-IMUN-ESTBL-1

           MOVE    INP-ENDER2-01TM       TO   WRK-JUNTA-END
           MOVE    INP-NUMERO2-01TM      TO   WRK-JUNTA-NRO
           MOVE    INP-COMPL2-01TM       TO   WRK-JUNTA-COMPL
           MOVE    WRK-JUNTA-ENDERECO    TO   WRK-E-EESTBL-AFLIA-1

           MOVE    INP-UF2-01TM          TO   WRK-E-CSGL-UF-1
           MOVE    INP-EMAIL1-01TM       TO   WRK-E-EEMAIL-CNTAT-ESTBL-1
           MOVE    INP-CONTAT1-01TM      TO   WRK-E-ICNTAT-ESTBL-AFLIA-1

      *------------- TERCEIRO ENDERECO -------
           MOVE    INP-CEPNRO3-01TM-N    TO   WRK-E-CCEP-2
           MOVE    INP-CEPSUF3-01TM-N    TO   WRK-E-CCEP-COMPL-2
           MOVE    INP-CIDADE3-01TM      TO   WRK-E-IMUN-ESTBL-2

           MOVE    INP-ENDER3-01TM       TO   WRK-JUNTA-END
           MOVE    INP-NUMERO3-01TM      TO   WRK-JUNTA-NRO
           MOVE    INP-COMPL3-01TM       TO   WRK-JUNTA-COMPL
           MOVE    WRK-JUNTA-ENDERECO    TO   WRK-E-EESTBL-AFLIA-2

           MOVE    INP-UF3-01TM          TO   WRK-E-CSGL-UF-2

      *------------ PRIMEIRO   TELEFONE  ----
           MOVE    INP-TDDD1-01TM        TO   WRK-E-CDDD-FONE-ESTBL-1
           MOVE    INP-TFONE1-01TM-N     TO   WRK-E-CFONE-PPRIE-1

      *------------ SEGUNDO  TELEFONE  ------
           MOVE    INP-FDDD1-01TM        TO   WRK-E-CDDD-FONE-ESTBL-2
           MOVE    INP-FFONE1-01TM-N     TO   WRK-E-CFONE-PPRIE-2

      *------------ PRIMEIRO  PROPRIETARIO --
           MOVE INP-PROPR1-01TM    TO  WRK-E-IPPRIE-ESTBL-AFLIA-1
           MOVE INP-CPFP1-01TM-N   TO  WRK-E-CCPF-PPRIE-1
           MOVE INP-DIGP1-01TM-N   TO  WRK-E-CCTRL-CPD-PPRIE-1
           MOVE INP-DDNASC1-01TM-N TO  WRK-DIA-DB2
           MOVE INP-MMNASC1-01TM-N TO  WRK-MES-DB2
           MOVE INP-AANASC1-01TM-N TO  WRK-ANO-DB2
           MOVE WRK-DATA-DB2       TO  WRK-E-DNASC-PPRIE-1

      *------------ SEGUNDO  PROPRIETARIO --

           IF      INP-PROPR2-01TM     NOT EQUAL       SPACES
                   MOVE INP-PROPR2-01TM    TO WRK-E-IPPRIE-ESTBL-AFLIA-2
                   MOVE INP-CPFP2-01TM-N   TO WRK-E-CCPF-PPRIE-2
                   MOVE INP-DIGP2-01TM-N   TO WRK-E-CCTRL-CPD-PPRIE-2
                   MOVE INP-DDNASC2-01TM-N TO WRK-DIA-DB2
                   MOVE INP-MMNASC2-01TM-N TO WRK-MES-DB2
                   MOVE INP-AANASC2-01TM-N TO WRK-ANO-DB2
                   MOVE WRK-DATA-DB2       TO WRK-E-DNASC-PPRIE-2
           ELSE
                   MOVE SPACES             TO WRK-E-IPPRIE-ESTBL-AFLIA-2
                   MOVE ZEROS              TO WRK-E-CCPF-REDEF-2
                                              WRK-E-CCTRL-RED-2
                   MOVE SPACES             TO WRK-E-DNASC-PPRIE-2
           END-IF.

      *------------- TERCEIRO PROPRIETARIO --

           IF      INP-PROPR3-01TM     NOT EQUAL       SPACES
                   MOVE INP-PROPR3-01TM    TO WRK-E-IPPRIE-ESTBL-AFLIA-3
                   MOVE INP-CPFP3-01TM-N   TO WRK-E-CCPF-PPRIE-3
                   MOVE INP-DIGP3-01TM-N   TO WRK-E-CCTRL-CPD-PPRIE-3
                   MOVE INP-DDNASC3-01TM-N TO WRK-DIA-DB2
                   MOVE INP-MMNASC3-01TM-N TO WRK-MES-DB2
                   MOVE INP-AANASC3-01TM-N TO WRK-ANO-DB2
                   MOVE WRK-DATA-DB2       TO WRK-E-DNASC-PPRIE-3
           ELSE
                   MOVE SPACES             TO WRK-E-IPPRIE-ESTBL-AFLIA-3
                   MOVE ZEROS              TO WRK-E-CCPF-REDEF-3
                                              WRK-E-CCTRL-RED-3
                   MOVE SPACES             TO WRK-E-DNASC-PPRIE-3
           END-IF.

      *-------- DEMAIS CAMPOS PROPOSTA --
           MOVE    INP-BANCO-01TM-N      TO   WRK-E-CBANCO
           MOVE    INP-AGENCIA-01TM-N    TO   WRK-E-CAG-BCRIA
           MOVE    INP-CONTA-01TM-N      TO   WRK-E-CCTA-BCRIA-CLI

           MOVE    'EXEC'                  TO      BVVE21-COMANDO.
           MOVE    'BVVE7001'              TO      BVVE21-MODULO.
           MOVE    WRK-ENVIA-DADOS-PROPOSTA-I TO   BVVE21-PARAMETROS.
           MOVE    759                     TO      BVVE21-TAM-REGISTRO.

           MOVE    'BVVE7920'              TO      WRK-MODULO.

           CALL    WRK-MODULO      USING     BVVE21-AREA-ENTRADA
                                             BVVE22-AREA-VARIAVEIS
                                             ERRO-AREA
                                             WRK-SQLCA.

JEM        IF      BVVE21-COD-RETORNO      NOT EQUAL   00 AND 02 AND 03
JEM                MOVE BVVE21-MODULO      TO ERR-MODULO
JEM                PERFORM     9999-ROTINA-ERRO
JEM        END-IF.

       2310-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       2400-DEVOLVE-TELA               SECTION.
      *----------------------------------------------------------------*

           MOVE    INP-SENHAS-01TM         TO      OUT-SENHAS-01TM.
           MOVE    INP-FASE-01TM           TO      OUT-FASE-01TM
           MOVE    INP-CHAMADOR-01TM       TO      OUT-CHAMADOR-01TM
           MOVE    INP-PAG2660-01TM        TO      OUT-PAG2660-01TM
           MOVE    WRK-INP-ATRIBS-01TM     TO      WRK-OUT-ATRIBS-01TM
           MOVE    INP-CNPJNRO-01TM        TO      OUT-CNPJNRO-01TM
JEM        MOVE    WRK-INP-INCALT-01TM     TO      WRK-OUT-INCALT-01TM
           MOVE    INP-CNPJFIL-01TM        TO      OUT-CNPJFIL-01TM
           MOVE    INP-CNPJCTR-01TM        TO      OUT-CNPJCTR-01TM
           MOVE    INP-NOMEPRO-01TM        TO      OUT-NOMEPRO-01TM

           IF INP-INSCRES-01TM EQUAL ZEROS
              MOVE SPACES                  TO      OUT-INSCRES-01TM
           ELSE
              MOVE INP-INSCRES-01TM        TO      OUT-INSCRES-01TM
           END-IF.

           MOVE    INP-RAMO-01TM           TO      OUT-RAMO-01TM
           MOVE    INP-IRAMO-01TM          TO      OUT-IRAMO-01TM
           MOVE    INP-NMFANT-01TM         TO      OUT-NMFANT-01TM
           MOVE    INP-NMPLAQ-01TM         TO      OUT-NMPLAQ-01TM


           MOVE    INP-CEPNRO1-01TM        TO      OUT-CEPNRO1-01TM
           MOVE    INP-CEPSUF1-01TM        TO      OUT-CEPSUF1-01TM
           MOVE    INP-ENDER1-01TM         TO      OUT-ENDER1-01TM
           MOVE    INP-NUMERO1-01TM        TO      OUT-NUMERO1-01TM
           MOVE    INP-COMPL1-01TM         TO      OUT-COMPL1-01TM
           MOVE    INP-CIDADE1-01TM        TO      OUT-CIDADE1-01TM
           MOVE    INP-UF1-01TM            TO      OUT-UF1-01TM

           MOVE    INP-CEPNRO2-01TM        TO      OUT-CEPNRO2-01TM
           MOVE    INP-CEPSUF2-01TM        TO      OUT-CEPSUF2-01TM
           MOVE    INP-ENDER2-01TM         TO      OUT-ENDER2-01TM
           MOVE    INP-NUMERO2-01TM        TO      OUT-NUMERO2-01TM
           MOVE    INP-COMPL2-01TM         TO      OUT-COMPL2-01TM
           MOVE    INP-CIDADE2-01TM        TO      OUT-CIDADE2-01TM
           MOVE    INP-UF2-01TM            TO      OUT-UF2-01TM

           MOVE    INP-TDDD1-01TM          TO      OUT-TDDD1-01TM
           MOVE    INP-TFONE1-01TM         TO      OUT-TFONE1-01TM
           MOVE    INP-FDDD1-01TM          TO      OUT-FDDD1-01TM
           MOVE    INP-FFONE1-01TM         TO      OUT-FFONE1-01TM
           MOVE    INP-CONTR1-01TM         TO      OUT-CONTR1-01TM
           MOVE    INP-CONTAT1-01TM        TO      OUT-CONTAT1-01TM
           MOVE    INP-EMAIL1-01TM         TO      OUT-EMAIL1-01TM

           MOVE    INP-CEPNRO3-01TM        TO      OUT-CEPNRO3-01TM
           MOVE    INP-CEPSUF3-01TM        TO      OUT-CEPSUF3-01TM
           MOVE    INP-ENDER3-01TM         TO      OUT-ENDER3-01TM
           MOVE    INP-NUMERO3-01TM        TO      OUT-NUMERO3-01TM
           MOVE    INP-COMPL3-01TM         TO      OUT-COMPL3-01TM
           MOVE    INP-CIDADE3-01TM        TO      OUT-CIDADE3-01TM
           MOVE    INP-UF3-01TM            TO      OUT-UF3-01TM

           MOVE    INP-PROPR1-01TM         TO      OUT-PROPR1-01TM
           MOVE    INP-CPFP1-01TM          TO      OUT-CPFP1-01TM
           MOVE    INP-DIGP1-01TM          TO      OUT-DIGP1-01TM
           MOVE    INP-DDNASC1-01TM        TO      OUT-DDNASC1-01TM
           MOVE    INP-MMNASC1-01TM        TO      OUT-MMNASC1-01TM
           MOVE    INP-AANASC1-01TM        TO      OUT-AANASC1-01TM.
           MOVE    INP-PROPR2-01TM         TO      OUT-PROPR2-01TM
           MOVE    INP-CPFP2-01TM          TO      OUT-CPFP2-01TM
           MOVE    INP-DIGP2-01TM          TO      OUT-DIGP2-01TM
           MOVE    INP-DDNASC2-01TM        TO      OUT-DDNASC2-01TM
           MOVE    INP-MMNASC2-01TM        TO      OUT-MMNASC2-01TM
           MOVE    INP-AANASC2-01TM        TO      OUT-AANASC2-01TM
           MOVE    INP-PROPR3-01TM         TO      OUT-PROPR3-01TM
           MOVE    INP-CPFP3-01TM          TO      OUT-CPFP3-01TM
           MOVE    INP-DIGP3-01TM          TO      OUT-DIGP3-01TM
           MOVE    INP-DDNASC3-01TM        TO      OUT-DDNASC3-01TM
           MOVE    INP-MMNASC3-01TM        TO      OUT-MMNASC3-01TM
           MOVE    INP-AANASC3-01TM        TO      OUT-AANASC3-01TM
           MOVE    INP-BANCO-01TM          TO      OUT-BANCO-01TM
           MOVE    INP-AGENCIA-01TM        TO      OUT-AGENCIA-01TM
           MOVE    INP-CONTA-01TM          TO      OUT-CONTA-01TM
           MOVE    INP-DIGCTA-01TM         TO      OUT-DIGCTA-01TM.

           IF  WRK-INCONSISTENTE   EQUAL ' '
               IF INP-FASE-01TM    EQUAL '1'
                   IF WRK-INP-INCALT-01TM EQUAL '*'
                       MOVE WRK-49361   TO WRK-OUT-ATTR-CNPJFIL-01TM
                                               WRK-OUT-ATTR-CNPJCTR-01TM
                   ELSE
                       MOVE WRK-225     TO WRK-OUT-ATTR-CNPJFIL-01TM
                                               WRK-OUT-ATTR-CNPJCTR-01TM
                   END-IF
               ELSE
                   MOVE WRK-225         TO WRK-OUT-ATTR-CNPJFIL-01TM
                                               WRK-OUT-ATTR-CNPJCTR-01TM
               END-IF
           END-IF.

           IF      INP-CNPJFIL-01TM-N      EQUAL       ZEROS
                   MOVE   WRK-225      TO      OUT-ATTR-PROPR1-01TM
                                               OUT-ATTR-CPFP1-01TM
                                               OUT-ATTR-PROPR2-01TM
                                               OUT-ATTR-CPFP2-01TM
                                               OUT-ATTR-DDNASC2-01TM
                                               OUT-ATTR-PROPR3-01TM
                                               OUT-ATTR-CPFP3-01TM
                                               OUT-ATTR-DDNASC3-01TM
           END-IF.


       2400-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       3000-POOL0660-BVVE01TM          SECTION.
      *----------------------------------------------------------------*

           CALL    'POOL0660'          USING       OUTPUT-01TM
                                                   660-BVVE01TM.

           IF      RETURN-CODE         NOT EQUAL       ZEROS
                   MOVE    'APL'               TO      ERR-TIPO-ACESSO
                   MOVE    RETURN-CODE         TO      WRK-RETURN-CODE
                   MOVE    10                  TO      WRK-LOCAL-ERRO
                   MOVE    WRK-ERRO-POOL0660   TO      ERR-TEXTO
                   PERFORM     9999-ROTINA-ERRO.

           MOVE    WRK-ISRT            TO      WRK-FUNCAO
           MOVE    OUTPUT-01TM         TO      WRK-MENSAGEM.

       3000-99-FIM.                 EXIT.

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'SENHAS02'             TO    ERR-PGM
           MOVE WRK-COD-USER           TO    ERR-COD-USER
           MOVE WRK-COD-DEPTO          TO    ERR-COD-DEPTO

           IF   ERR-TIPO-ACESSO     EQUAL    'APL'
                CALL 'POOL7100'     USING    IO-PCB



                                             ERRO-AREA
                                             ALT-PCB
           ELSE
                CALL 'POOL7100'     USING    IO-PCB
                                             ERRO-AREA
                                             ALT-PCB
                                             WRK-SQLCA.
           GOBACK.

       9999-99-FIM.                 EXIT.
