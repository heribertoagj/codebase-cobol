      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID.    BVVE0542.
       AUTHOR.        FRANCISMARA.

      *================================================================*
      *                   B S I  T E C N O L O G I A                   *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMADOR  : FRANCISMARA         - BSI                  *
      *      ANALISTA     : FRANCISMARA         - BSI                  *
      *      DATA         : MARCO/2017                                 *
      *                                                                *
      *      OBJETIVO     :                                            *
      *        OBTER NUMERO DE ESTABELECIMENTO E CNPJ PARA O ARQUIVO   *
      *        DE CONTAS COM TRAG DA REDE - COM TRAVA                  *
      *                                                                *
      *      ARQUIVOS                                                  *
      *                                                                *
      *      INPUT :                                                   *
      *                                                                *
      *      TDBCOMTR            - CLIENTES COM TRAG EM CONTA          *
      *      EREDEAFI            - CADASTRO DE AFILIACOES REDE         *
      *      BANDEIRA            - INDICA AS BANDEIRAS DE CADA AGENCIA *
      *                            E CONTA                             *
      *      OUTPUT :                                                  *
      *                                                                *
      *      SREDETDB            - ARQUIVO DE TRAG COM ESTABELECIMENTO *
      *                            E CNPJ                              *
      *      RELAFIL             - RELATORIO DE AFILIACOES             *
      *                                                                *
      *================================================================*

      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.

           SELECT  TDBCOMTR   ASSIGN  TO  UT-S-TDBCOMTR
                                  FILE STATUS IS WRK-FS-TDBCOMTR.

           SELECT  EREDEAFI   ASSIGN  TO  UT-S-EREDEAFI
                                  FILE STATUS IS WRK-FS-EREDEAFI.

           SELECT  SREDETDB   ASSIGN  TO  UT-S-SREDETDB
                                  FILE STATUS IS WRK-FS-SREDETDB.

           SELECT  BANDEIRA   ASSIGN  TO  UT-S-BANDEIRA
                                  FILE STATUS IS WRK-FS-BANDEIRA.

           SELECT  RELAFIL    ASSIGN  TO  UT-S-RELAFIL
                                  FILE STATUS IS WRK-FS-RELAFIL .

      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CLIENTES COM TRAG EM CONTA              - ENTRADA LRECL = 080  *
      *----------------------------------------------------------------*

       FD  TDBCOMTR
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-TDBCOMTR             PIC X(080).

      *----------------------------------------------------------------*
      * CADASTRO DE AFILIACOES REDE             - ENTRADA LRECL = 200  *
      *----------------------------------------------------------------*

       FD  EREDEAFI
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-EREDEAFI             PIC X(200).

      *----------------------------------------------------------------*
      * LISTA OS PRODUTOS DE CADA AGENCIA E CONTA - ENTRADA - LRELC 50 *
      *----------------------------------------------------------------*

       FD  BANDEIRA
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-BANDEIRA             PIC X(050).

      *----------------------------------------------------------------*
      * ARQUIVO DE TRAG COM ESTABELECIMENTO     - SAIDA   LRECL = 100  *
      *----------------------------------------------------------------*

       FD  SREDETDB
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-SREDETDB             PIC X(100).

      *----------------------------------------------------------------*
      * LISTA CLIENTES COM AFILIACOES           - SAIDA   LRECL = 133  *
      *----------------------------------------------------------------*

       FD  RELAFIL
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-REG-RELAFIL              PIC X(133).

      ******************************************************************
       WORKING-STORAGE                 SECTION.
      ******************************************************************

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** INICIO DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FS-TDBCOMTR             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-EREDEAFI             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-SREDETDB             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-BANDEIRA             PIC  X(02)          VALUE SPACES.
       77  WRK-FS-RELAFIL              PIC  X(02)          VALUE SPACES.

       77  WRK-BATCH                   PIC  X(08)          VALUE
           'BATCH'.
       77  WRK-FECHAMENTO              PIC  X(13)          VALUE
           'NO FECHAMENTO'.
       77  WRK-ABERTURA                PIC  X(13)          VALUE
           'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13)          VALUE
           'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13)          VALUE
           'NA GRAVACAO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE MENSAGENS DE ERRO - FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(08)          VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           03  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(18)          VALUE
               ' - FILE STATUS =  '.
           03  WRK-FILE-STATUS         PIC  X(02)          VALUE SPACES.
           03  FILLER                  PIC  X(03)          VALUE ' **'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-ARQ-VAZIO               PIC  X(08)          VALUE SPACES.
       01  WRK-IND                     PIC  9(03) COMP-3   VALUE ZEROS.
       01  WRK-PRIM-VEZ                PIC  X(01)          VALUE SPACES.

7C2511*    01  WRK-CNPJ-CPF-AUX            PIC  9(15)          VALUE ZEROS.
7C2511 01  WRK-CNPJ-CPF-AUX            PIC  X(15)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-CNPJ-CPF-AUX.
7C2511*        05  WRK-NRO-CNPJ-AUX        PIC  9(09).
7C2511     05  WRK-NRO-CNPJ-AUX        PIC  X(09).
7C2511*        05  WRK-FILIAL-AUX          PIC  9(04).
7C2511     05  WRK-FILIAL-AUX          PIC  X(04).
           05  WRK-CTRL-CNPJ-AUX       PIC  9(02).
       01  FILLER                      REDEFINES  WRK-CNPJ-CPF-AUX.
           05  FILLER                  PIC  X(04).
7C2511*        05  WRK-NRO-CPF-AUX         PIC  9(09).
7C2511     05  WRK-NRO-CPF-AUX         PIC  X(09).
           05  WRK-CTRL-CPF-AUX        PIC  9(02).

       01  WRK-NUM-CNPJ-EDT.
7C2511*        03 WRK-CNPJ-EDT             PIC 9(08)         VALUE ZEROS.
7C2511     03 WRK-CNPJ-EDT             PIC X(08)         VALUE SPACES.
           03 FILLER                   PIC X(01)         VALUE '/'.
7C2511*        03 WRK-FILIAL-EDT           PIC 9(04)         VALUE ZEROS.
7C2511     03 WRK-FILIAL-EDT           PIC X(04)         VALUE SPACES.
           03 FILLER                   PIC X(01)         VALUE '-'.
           03 WRK-CNPJ-CTR-EDT         PIC 9(02)         VALUE ZEROS.

7C2511*    01  WRK-NUM-CNPJ                PIC 9(15)         VALUE ZEROS.
7C2511 01  WRK-NUM-CNPJ                PIC X(15)         VALUE SPACES.
       01  FILLER REDEFINES WRK-NUM-CNPJ.
7C2511*        03 WRK-CNPJ                 PIC 9(09).
7C2511     03 WRK-CNPJ                 PIC X(09).
7C2511*        03 WRK-FILIAL               PIC 9(04).
7C2511     03 WRK-FILIAL               PIC X(04).
           03 WRK-CNPJ-CTR             PIC 9(02).

       01  WRK-TAB.
           03  WRK-TAB-BAND            PIC X(02)         VALUE SPACES
                                       OCCURS 20.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA TRATAMENTO DE ERRO ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUT DO ARQUIVO TDBCOMTR ***'.
      *----------------------------------------------------------------*
       01  WRK-REG-TDBCOMTR.
           05  WRK-ERED-AGENCOLD       PIC  9(05)  COMP-3  VALUE ZEROS.
           05  WRK-ERED-CONTAOLD       PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-ERED-DGCTAOLD       PIC  X(01)          VALUE SPACES.
           05  WRK-ERED-CREDENCIADORA  PIC  X(20)          VALUE SPACES.
           05  WRK-ERED-CODIGO-TDB     PIC  X(02)          VALUE SPACES.
           05  WRK-ERED-AGENCNEW       PIC  9(05)  COMP-3  VALUE ZEROS.
           05  WRK-ERED-CONTANEW       PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-ERED-DGCTANEW       PIC  X(01)          VALUE SPACES.
           05  WRK-ERED-BAND-CIP       PIC  X(03)          VALUE SPACES.
7C2511*        05  WRK-ERED-CNPJ           PIC  9(09)          VALUE ZEROS.
7C2511     05  WRK-ERED-CNPJ           PIC  X(09)          VALUE SPACES.
7C2511*        05  WRK-ERED-FILIAL         PIC  9(04)          VALUE ZEROS.
7C2511     05  WRK-ERED-FILIAL         PIC  X(04)          VALUE SPACES.
           05  WRK-ERED-CTRL           PIC  X(02)          VALUE SPACES.
           05  WRK-ERED-RAIZ           PIC  X(01)          VALUE SPACES.
           05  WRK-ERED-PESSOA         PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(22)          VALUE SPACES.

       01  WRK-REG-TDBCOMTR-ANT.
           05  WRK-ANT-AGENCOLD       PIC  9(05)  COMP-3  VALUE ZEROS.
           05  WRK-ANT-CONTAOLD       PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-ANT-DGCTAOLD       PIC  X(01)          VALUE SPACES.
           05  WRK-ANT-CREDENCIADORA  PIC  X(20)          VALUE SPACES.
           05  WRK-ANT-CODIGO-TDB     PIC  X(02)          VALUE SPACES.
           05  WRK-ANT-AGENCNEW       PIC  9(05)  COMP-3  VALUE ZEROS.
           05  WRK-ANT-CONTANEW       PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-ANT-DGCTANEW       PIC  X(01)          VALUE SPACES.
           05  WRK-ANT-BAND-CIP       PIC  X(03)          VALUE SPACES.
7C2511*        05  WRK-ANT-CNPJ           PIC  9(09)          VALUE ZEROS.
7C2511     05  WRK-ANT-CNPJ           PIC  X(09)          VALUE SPACES.
7C2511*        05  WRK-ANT-FILIAL         PIC  9(04)          VALUE ZEROS.
7C2511     05  WRK-ANT-FILIAL         PIC  X(04)          VALUE SPACES.
           05  WRK-ANT-CTRL           PIC  X(02)          VALUE SPACES.
           05  WRK-ANT-RAIZ           PIC  X(01)          VALUE SPACES.
           05  WRK-ANT-PESSOA         PIC  X(01)          VALUE SPACES.
           05  FILLER                 PIC  X(22)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUTS DO ARQUIVO EREDEAFI ***'.
      *----------------------------------------------------------------*

       01  WRK-REG-EREDEAFI.
           05  WRK-AFIL-TIPO-REG       PIC  9(02)          VALUE ZEROS.
           05  WRK-AFIL-COD-CANAL      PIC  9(02)          VALUE ZEROS.
           05  WRK-AFIL-COD-BCO        PIC  9(04)          VALUE ZEROS.
           05  WRK-AFIL-DATA-PROC      PIC  9(08)          VALUE ZEROS.
      *        AAAAMMDD
           05  WRK-AFIL-HORA-PROC      PIC  9(06)          VALUE ZEROS.
      *        HHMMSS
           05  WRK-AFIL-APURACAO       PIC  9(06)          VALUE ZEROS.
      *        AAAAMM
           05  WRK-AFIL-ESTAB          PIC  9(09)          VALUE ZEROS.
           05  WRK-AFIL-TIPO-TECNO     PIC  X(01)          VALUE SPACES.
           05  WRK-AFIL-NOME-COMERCIAL PIC  X(30)          VALUE SPACES.
7C2511*        05  WRK-AFIL-CNPJ-CPF       PIC  9(15)          VALUE ZEROS.
7C2511     05  WRK-AFIL-CNPJ-CPF       PIC  X(15)          VALUE SPACES.
           05  WRK-AFIL-AGE-DOMIC-CRED PIC  9(04)          VALUE ZEROS.
           05  WRK-AFIL-CTA-DOMIC-CRED PIC  X(10)          VALUE SPACES.
           05  FILLER  REDEFINES  WRK-AFIL-CTA-DOMIC-CRED.
               10  FILLER              PIC  X(02).
               10  WRK-AFIL-CTA-DOMIC-CRED-R
                                       PIC  9(07).
               10  WRK-AFIL-DG-CTA-DOMIC-CRED
                                       PIC  X(01).
           05  WRK-AFIL-VLR-FAT-CRED   PIC  9(13)V99       VALUE ZEROS.
           05  WRK-AFIL-VLR-FAT-DEB    PIC  9(13)V99       VALUE ZEROS.
           05  WRK-AFIL-DATA-FILIACAO  PIC  9(08)          VALUE ZEROS.
      *        AAAAMMDD
           05  WRK-AFIL-RAMO-ATIV      PIC  9(05)          VALUE ZEROS.
           05  WRK-AFIL-DOMIC-CRED     PIC  X(01)          VALUE SPACES.
           05  WRK-AFIL-DOMIC-DEB      PIC  X(01)          VALUE SPACES.
           05  WRK-AFIL-CANAL-FILIACAO PIC  9(02)          VALUE ZEROS.
           05  WRK-AFIL-CELULA-BCO     PIC  9(04)          VALUE ZEROS.
           05  WRK-AFIL-PRIMEIRA-TEC   PIC  X(03)          VALUE SPACES.
           05  WRK-AFIL-TIPO-PESSOA    PIC  X(01)          VALUE SPACES.
      *        F = FISICA - J = JURIDICA
           05  WRK-AFIL-AGE-DOMIC-DEB  PIC  9(04)          VALUE ZEROS.
           05  WRK-AFIL-CTA-DOMIC-DEB  PIC  X(10)          VALUE SPACES.
           05  WRK-AFIL-TECNOLOGIA     PIC  X(30)          VALUE SPACES.
           05  FILLER                  PIC  X(04)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUT DO ARQUIVO SREDETDB ***'.
      *----------------------------------------------------------------*

       01  WRK-REG-SREDETDB.
           05  WRK-SRED-AGENCOLD       PIC  9(05)  COMP-3  VALUE ZEROS.
           05  WRK-SRED-CONTAOLD       PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-SRED-DGCTAOLD       PIC  X(01)          VALUE SPACES.
           05  WRK-SRED-CREDENCIADORA  PIC  X(20)          VALUE SPACES.
           05  WRK-SRED-CODIGO-TDB     PIC  X(02)          VALUE SPACES.
           05  WRK-SRED-AGENCNEW       PIC  9(05)  COMP-3  VALUE ZEROS.
           05  WRK-SRED-CONTANEW       PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-SRED-DGCTANEW       PIC  X(01)          VALUE SPACES.
           05  WRK-SRED-ESTABELECIMENTO
                                       PIC  9(10)  COMP-3  VALUE ZEROS.
7C2511*        05  WRK-SRED-CNPJ-CPF       PIC  9(09)          VALUE ZEROS.
7C2511     05  WRK-SRED-CNPJ-CPF       PIC  X(09)          VALUE SPACES.
7C2511*        05  WRK-SRED-FILIAL         PIC  9(04)          VALUE ZEROS.
7C2511     05  WRK-SRED-FILIAL         PIC  X(04)          VALUE SPACES.
           05  WRK-SRED-CTRL           PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(41)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUT DO ARQUIVO BANDEIRA ***'.
      *----------------------------------------------------------------*

       01  WRK-REG-BANDEIRA.
           05  WRK-BAND-AGENCOLD       PIC  9(05)  COMP-3  VALUE ZEROS.
           05  WRK-BAND-CONTAOLD       PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-BAND-DGCTAOLD       PIC  X(01)          VALUE SPACES.
           05  WRK-BAND-CREDENCIADORA  PIC  X(20)          VALUE SPACES.
           05  WRK-BAND-CODIGO-TDB     PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(20)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-LIDOS-TDBCOMTR          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-EREDEAFI          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-SREDETDB          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-BANDEIRA          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-RELAFIL           PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-QTD-COM-AFILIACAO       PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-QTD-SEM-AFILIACAO       PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-PAGINAS-2               PIC  9(05)  COMP-3  VALUE ZEROS.
       01  ACU-LINHAS-2                PIC  9(02)  COMP-3  VALUE 99.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** MASCARA DE EDICAO ***'.
      *----------------------------------------------------------------*

       01  WRK-MASC                    PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** CHAVES DE COMPARACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-TDBCOMTR.
           05  WRK-CH-ERED-AGENCIA     PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-ERED-CONTA       PIC  9(07)          VALUE ZEROS.

       01  WRK-CHAVE-BANDEIRA.
           05  WRK-CH-BAND-AGENCIA     PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-BAND-CONTA       PIC  9(07)          VALUE ZEROS.

       01  WRK-CHAVE-EREDEAFI.
           05  WRK-CH-AFIL-AGENCIA     PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-AFIL-CONTA       PIC  9(07)          VALUE ZEROS.

       01  WRK-CHAVE-ANTERIOR.
           05  WRK-CH-AGENCIA-ANT      PIC  9(05)          VALUE ZEROS.
           05  WRK-CH-CONTA-ANT        PIC  9(07)          VALUE ZEROS.

       01  WRK-CHV-CNPJ-TDB.
7C2511*        05  WRK-CH-CNPJ-TDB         PIC  9(09)          VALUE ZEROS.
7C2511     05  WRK-CH-CNPJ-TDB         PIC  X(09)          VALUE SPACES.
7C2511*        05  WRK-CH-FILIA-TDB        PIC  9(05)          VALUE ZEROS.
7C2511     05  WRK-CH-FILIA-TDB        PIC  X(04)          VALUE SPACES.
           05  WRK-CH-CTRL-TDB         PIC  X(02)          VALUE SPACES.

       01  WRK-CHV-CNPJ-AFIL.
7C2511*        05  WRK-CH-CNPJ-AFI         PIC  9(09)          VALUE ZEROS.
7C2511     05  WRK-CH-CNPJ-AFI         PIC  X(09)          VALUE SPACES.
7C2511*        05  WRK-CH-FILIA-AFI        PIC  9(05)          VALUE ZEROS.
7C2511     05  WRK-CH-FILIA-AFI        PIC  X(04)          VALUE SPACES.
           05  WRK-CH-CTRL-AFI         PIC  9(02)          VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DT-JULIANA          PIC  9(05) COMP-3   VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC  9(09) COMP-3   VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-TI-HHMMSSMMM        PIC  9(13) COMP-3   VALUE ZEROS.
           05  WRK-TIMESTAMP           PIC  X(20)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DATA/HORA DE COMPILACAO E EXECUCAO ***'.
      *----------------------------------------------------------------*

       77  WRK-DT-COMPILED             PIC  X(16)          VALUE SPACES.
       77  WRK-DS-COMPILED             PIC  X(08)BBBX(08)  VALUE SPACES.

       01  WRK-DATA-SISTEMA            PIC  9(09)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-DATA-SISTEMA.
           05  FILLER                  PIC  X(01).
           05  WRK-SEC-ANO-SISTEMA     PIC  9(04).
           05  FILLER                  REDEFINES  WRK-SEC-ANO-SISTEMA.
               10  WRK-SEC-SISTEMA     PIC  9(02).
               10  WRK-ANO-SISTEMA     PIC  9(02).
           05  WRK-MES-SISTEMA         PIC  9(02).
           05  WRK-DIA-SISTEMA         PIC  9(02).

       01  WRK-DATA-EXECUCAO.
           05  WRK-DD-EXECUCAO         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  WRK-MM-EXECUCAO         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  WRK-AA-EXECUCAO         PIC  9(02)          VALUE ZEROS.

       01  WRK-HORA-SISTEMA            PIC  9(07)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-HORA-SISTEMA.
           05  FILLER                  PIC  X(01).
           05  WRK-HH-SISTEMA          PIC  9(02).
           05  WRK-MM-SISTEMA          PIC  9(02).
           05  WRK-SS-SISTEMA          PIC  9(02).

       01  WRK-HORA-EXECUCAO.
           05  WRK-HOR-EXECUCAO        PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-MIN-EXECUCAO        PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-SEG-EXECUCAO        PIC  9(02)          VALUE ZEROS.

       01  WRK-LITERAL                 PIC  X(18)          VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** LAYOUT DO ARQUIVO BANDEIRA ***'.
      *----------------------------------------------------------------*

       01  CABEC1-REL1-BANDEIRA.
           05  FILLER                  PIC  X(01)          VALUE '1'.
           05  FILLER                  PIC  X(24)          VALUE
           'BVVE0542'.
           05  FILLER                  PIC  X(46)          VALUE
           'B R A D E S C O   C A R T O E S'.
           05  FILLER                  PIC  X(04)          VALUE 'PAG.'.
           05  CB1-REL1-PAG            PIC  ZZ.999         VALUE ZEROS.

       01  CABEC2-REL1-BANDEIRA.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  CB2-REL1-DD             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  CB2-REL1-MM             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '/'.
           05  CB2-REL1-AAAA           PIC  9(04)          VALUE ZEROS.
           05  FILLER                  PIC  X(17)          VALUE SPACES.
           05  FILLER                  PIC  X(45)          VALUE
           'TRAG NA AGENDA FINANCEIRA'.
           05  CB2-REL1-HH             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE ':'.
           05  CB2-REL1-MI             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE ':'.
           05  CB2-REL1-SS             PIC  9(02)          VALUE ZEROS.

       01  CABEC3-REL2-RELAFIL.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(10)          VALUE SPACES.
           05  FILLER                  PIC  X(70)          VALUE
           'ESTABELECIMENTOS AFILIADOS SEM TRAVA DE DOMICILIO'.

       01  CABEC4-REL1-BANDEIRA.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(59)          VALUE
           '------ ANTIGA ------'.
           05  FILLER                  PIC  X(20)          VALUE
           '------- NOVA -------'.

       01  CABEC5-REL2-RELAFIL.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(111)         VALUE
           'AGENCIA      CONTA      CREDENCIADORA        CODIGO TDB    A
      -    'GENCIA      CONTA     ESTABELECIMENTO      CNPJ/CPF'.

       01  CABEC7-REL1-LINBRANCO.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(80)          VALUE SPACES.

       01  LINDET1-REL2-RELAFIL.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  LD1-REL2-AGENCOLD       PIC  Z9999          VALUE ZEROS.
           05  FILLER                  PIC  X(05)          VALUE SPACES.
           05  LD1-REL2-CONTAOLD       PIC  ZZZZZZ9        VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '-'.
           05  LD1-REL2-DGCTAOLD       PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(04)          VALUE SPACES.
           05  LD1-REL2-CREDENCIADORA  PIC  X(20)          VALUE SPACES.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  LD1-REL2-CODIGO-TDB     PIC  X(02)          VALUE SPACES.
           05  FILLER                  PIC  X(01)          VALUE '-'.
           05  LD1-REL2-DESC-TDB       PIC  X(07)          VALUE SPACES.
           05  FILLER                  PIC  X(05)          VALUE SPACES.
           05  LD1-REL2-AGENCNEW       PIC  Z9999          VALUE ZEROS.
           05  FILLER                  PIC  X(05)          VALUE SPACES.
           05  LD1-REL2-CONTANEW       PIC  ZZZZZZ9        VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '-'.
           05  LD1-REL2-DGCTANEW       PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(05)          VALUE SPACES.
           05  LD1-REL2-ESTAB          PIC  ZZZZZZZZ9      VALUE ZEROS.
           05  FILLER                  PIC  X(06)          VALUE SPACES.
           05  LD1-REL2-CNPJ           PIC  X(16)          VALUE SPACES.

       01  LINTOT1-REL1-BANDEIRA.
           05  FILLER                  PIC  X(01)          VALUE SPACES.
           05  FILLER                  PIC  X(13)          VALUE
           'TOTAL GERAL: '.
           05  LT1-REL1-QTD-CIELO      PIC  ZZ.ZZZ.ZZZ.999 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** FIM DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM  1000-INICIO
           PERFORM  5000-PROCESSO
                   UNTIL  WRK-FS-EREDEAFI  EQUAL  '10'.
           PERFORM  8000-FINALIZA

           STOP RUN.

      *----------------------------------------------------------------*
       0000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           MOVE  'INICIO DA EXECUCAO'  TO  WRK-LITERAL
           PERFORM 9998-MONTA-DISPLAY

           OPEN  INPUT  TDBCOMTR
                        EREDEAFI
                        BANDEIRA
                 OUTPUT SREDETDB
                        RELAFIL.

           MOVE  WRK-ABERTURA                   TO  WRK-OPERACAO

           PERFORM  1100-TESTAR-FILE-STATUS
           PERFORM  1200-TRATAR-PRIMEIRA-LEITURA.

           MOVE  WRK-DIA-SISTEMA       TO  CB2-REL1-DD
           MOVE  WRK-MES-SISTEMA       TO  CB2-REL1-MM
           MOVE  WRK-SEC-ANO-SISTEMA   TO  CB2-REL1-AAAA

           MOVE  WRK-HH-SISTEMA        TO  CB2-REL1-HH
           MOVE  WRK-MM-SISTEMA        TO  CB2-REL1-MI
           MOVE  WRK-SS-SISTEMA        TO  CB2-REL1-SS.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM  1110-TESTAR-FS-TDBCOMTR
           PERFORM  1120-TESTAR-FS-EREDEAFI
           PERFORM  1130-TESTAR-FS-SREDETDB
           PERFORM  1140-TESTAR-FS-BANDEIRA.
           PERFORM  1150-TESTAR-FS-RELAFIL.

      *----------------------------------------------------------------*
       1100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-TDBCOMTR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-TDBCOMTR        NOT  EQUAL '00'
               MOVE 'TDBCOMTR'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-TDBCOMTR    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-EREDEAFI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EREDEAFI        NOT  EQUAL '00'
               MOVE 'EREDEAFI'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-EREDEAFI    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-SREDETDB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SREDETDB        NOT  EQUAL '00'
               MOVE 'SREDETDB'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-SREDETDB    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-BANDEIRA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-BANDEIRA        NOT  EQUAL '00'
               MOVE 'BANDEIRA'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-BANDEIRA    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-TESTAR-FS-RELAFIL          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELAFIL         NOT  EQUAL '00'
               MOVE 'RELAFIL '         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-RELAFIL     TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TRATAR-PRIMEIRA-LEITURA    SECTION.
      *----------------------------------------------------------------*

           PERFORM  1210-LER-TDBCOMTR

           IF  WRK-FS-TDBCOMTR      EQUAL  '10'   AND
               ACU-LIDOS-TDBCOMTR   EQUAL   ZEROS
               MOVE  'TDBCOMTR'        TO  WRK-ARQ-VAZIO
               PERFORM  1298-DISPLAY-ARQ-VAZIO
           END-IF.

           PERFORM  1220-LER-EREDEAFI

           IF  WRK-FS-EREDEAFI      EQUAL  '10'   AND
               ACU-LIDOS-EREDEAFI   EQUAL   ZEROS
               MOVE  'EREDEAFI'        TO  WRK-ARQ-VAZIO
               PERFORM  1298-DISPLAY-ARQ-VAZIO
           END-IF.

           PERFORM  1230-LER-BANDEIRA

           IF  WRK-FS-BANDEIRA      EQUAL  '10'   AND
               ACU-LIDOS-BANDEIRA   EQUAL   ZEROS
               MOVE  'BANDEIRA'        TO  WRK-ARQ-VAZIO
               PERFORM  1298-DISPLAY-ARQ-VAZIO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-LER-TDBCOMTR               SECTION.
      *----------------------------------------------------------------*

           READ  TDBCOMTR            INTO  WRK-REG-TDBCOMTR

           IF  WRK-FS-TDBCOMTR  EQUAL  '10'
               MOVE  HIGH-VALUE        TO  WRK-CHAVE-TDBCOMTR
                                           WRK-CHV-CNPJ-TDB
           ELSE
               MOVE  WRK-LEITURA       TO  WRK-OPERACAO
               PERFORM  1110-TESTAR-FS-TDBCOMTR
               ADD   1                 TO  ACU-LIDOS-TDBCOMTR

               MOVE  WRK-ERED-AGENCOLD TO  WRK-CH-ERED-AGENCIA
               MOVE  WRK-ERED-CONTAOLD TO  WRK-CH-ERED-CONTA
               MOVE   WRK-ERED-CNPJ    TO  WRK-CH-CNPJ-TDB
               MOVE   WRK-ERED-FILIAL  TO  WRK-CH-FILIA-TDB
               MOVE   WRK-ERED-CTRL    TO  WRK-CH-CTRL-TDB

               IF  WRK-ERED-RAIZ       EQUAL 'S' OR
                   WRK-ERED-PESSOA     EQUAL 'F'
                   GO TO  1210-LER-TDBCOMTR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-LER-EREDEAFI               SECTION.
      *----------------------------------------------------------------*

           READ  EREDEAFI            INTO  WRK-REG-EREDEAFI

           IF  WRK-FS-EREDEAFI  EQUAL  '10'
               MOVE  HIGH-VALUE        TO  WRK-CHAVE-EREDEAFI
                                           WRK-CHV-CNPJ-AFIL
           ELSE
               MOVE  WRK-LEITURA       TO  WRK-OPERACAO
               PERFORM  1120-TESTAR-FS-EREDEAFI
               ADD   1                 TO  ACU-LIDOS-EREDEAFI

               MOVE  WRK-AFIL-AGE-DOMIC-CRED
                                       TO  WRK-CH-AFIL-AGENCIA
               MOVE  WRK-AFIL-CTA-DOMIC-CRED-R
                                       TO  WRK-CH-AFIL-CONTA

               MOVE   WRK-AFIL-CNPJ-CPF    TO  WRK-NUM-CNPJ
               MOVE   WRK-CNPJ             TO  WRK-CH-CNPJ-AFI
               MOVE   WRK-FILIAL           TO  WRK-CH-FILIA-AFI
               MOVE   WRK-CNPJ-CTR         TO  WRK-CH-CTRL-AFI

               IF  WRK-AFIL-TIPO-PESSOA    EQUAL 'F'
                   GO TO  1220-LER-EREDEAFI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-LER-BANDEIRA               SECTION.
      *----------------------------------------------------------------*

           READ  BANDEIRA              INTO  WRK-REG-BANDEIRA

           IF  WRK-FS-BANDEIRA  EQUAL  '10'
               MOVE  HIGH-VALUE        TO  WRK-CHAVE-BANDEIRA
           ELSE
               MOVE  WRK-LEITURA       TO  WRK-OPERACAO
               PERFORM  1140-TESTAR-FS-BANDEIRA
               ADD   1                 TO  ACU-LIDOS-BANDEIRA

               MOVE  WRK-BAND-AGENCOLD TO  WRK-CH-BAND-AGENCIA
               MOVE  WRK-BAND-CONTAOLD TO  WRK-CH-BAND-CONTA

           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1298-DISPLAY-ARQ-VAZIO          SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************** BVVE0542 ******************'
           DISPLAY '*                                            *'
           DISPLAY '*        ARQUIVO ' WRK-ARQ-VAZIO
                                            ' ESTA VAZIO         *'
           DISPLAY '*                                            *'
           DISPLAY '****************** BVVE0542 ******************'.

      *----------------------------------------------------------------*
       1298-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-PROCESSO                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-TDBCOMTR          EQUAL  WRK-CHAVE-EREDEAFI
               MOVE WRK-CHAVE-EREDEAFI     TO WRK-CHAVE-ANTERIOR
               MOVE WRK-REG-TDBCOMTR       TO WRK-REG-TDBCOMTR-ANT
               MOVE 'S'                    TO WRK-PRIM-VEZ

               PERFORM  5100-TRATAR-IGUAL
                 UNTIL  WRK-CHAVE-EREDEAFI NOT EQUAL WRK-CHAVE-ANTERIOR

           ELSE
               IF  WRK-CHAVE-TDBCOMTR      LESS  WRK-CHAVE-EREDEAFI
                   PERFORM  1210-LER-TDBCOMTR
               ELSE
                   PERFORM  1220-LER-EREDEAFI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-TRATAR-IGUAL               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHV-CNPJ-TDB        EQUAL WRK-CHV-CNPJ-AFIL
               PERFORM 1210-LER-TDBCOMTR
               PERFORM 1220-LER-EREDEAFI
           ELSE
               IF  WRK-CHV-CNPJ-TDB    LESS WRK-CHV-CNPJ-AFIL
                   PERFORM 1210-LER-TDBCOMTR
               ELSE
                   PERFORM 5110-GRAVAR-SAIDA
                   PERFORM 1220-LER-EREDEAFI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5110-GRAVAR-SAIDA                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-PRIM-VEZ                EQUAL 'S'
               MOVE ZEROS                  TO WRK-IND
               MOVE SPACES                 TO WRK-TAB
               MOVE SPACES                 TO WRK-PRIM-VEZ
               PERFORM 5111-CARREGAR-BANDEIRA
           END-IF.

           MOVE ZEROS                      TO WRK-IND

           PERFORM 5120-GRAVAR-SREDETDB
                   VARYING WRK-IND         FROM 1 BY 1
                     UNTIL WRK-IND         GREATER 20 OR
                     WRK-TAB-BAND(WRK-IND) EQUAL SPACES.

      *----------------------------------------------------------------*
       5110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5111-CARREGAR-BANDEIRA              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-BANDEIRA          EQUAL WRK-CHAVE-EREDEAFI
               ADD 1                       TO WRK-IND
               MOVE WRK-BAND-CODIGO-TDB    TO WRK-TAB-BAND(WRK-IND)
               PERFORM  1230-LER-BANDEIRA
               GO TO 5111-CARREGAR-BANDEIRA
           ELSE
               IF  WRK-CHAVE-BANDEIRA      LESS WRK-CHAVE-EREDEAFI
                   PERFORM  1230-LER-BANDEIRA
                   GO TO 5111-CARREGAR-BANDEIRA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5111-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5120-GRAVAR-SREDETDB            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                    WRK-REG-SREDETDB

           MOVE  WRK-ANT-AGENCOLD      TO  WRK-SRED-AGENCOLD
           MOVE  WRK-ANT-CONTAOLD      TO  WRK-SRED-CONTAOLD
           MOVE  WRK-ANT-DGCTAOLD      TO  WRK-SRED-DGCTAOLD
           MOVE  WRK-ANT-CREDENCIADORA TO  WRK-SRED-CREDENCIADORA
           MOVE  WRK-ANT-AGENCNEW      TO  WRK-SRED-AGENCNEW
           MOVE  WRK-ANT-CONTANEW      TO  WRK-SRED-CONTANEW
           MOVE  WRK-ANT-DGCTANEW      TO  WRK-SRED-DGCTANEW

           MOVE  WRK-AFIL-ESTAB         TO  WRK-SRED-ESTABELECIMENTO
           MOVE  WRK-AFIL-CNPJ-CPF      TO  WRK-CNPJ-CPF-AUX
           MOVE  WRK-NRO-CNPJ-AUX       TO  WRK-SRED-CNPJ-CPF
           MOVE  WRK-FILIAL-AUX         TO  WRK-SRED-FILIAL
           MOVE  WRK-CTRL-CNPJ-AUX      TO  WRK-SRED-CTRL
           MOVE  WRK-TAB-BAND(WRK-IND)  TO  WRK-SRED-CODIGO-TDB
           MOVE  WRK-GRAVACAO           TO  WRK-OPERACAO

           WRITE  FD-REG-SREDETDB      FROM WRK-REG-SREDETDB
           PERFORM  1130-TESTAR-FS-SREDETDB
           ADD    1                    TO  ACU-GRAVA-SREDETDB.

           PERFORM 5300-RELATO-ESTABELE.

      *----------------------------------------------------------------*
       5120-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5300-RELATO-ESTABELE             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS-2            GREATER  58
               PERFORM  5310-IMPRIMIR-CABECS
           END-IF.

           MOVE  WRK-SRED-AGENCOLD      TO  LD1-REL2-AGENCOLD
           MOVE  WRK-SRED-CONTAOLD      TO  LD1-REL2-CONTAOLD
           MOVE  WRK-SRED-DGCTAOLD      TO  LD1-REL2-DGCTAOLD
           MOVE  WRK-SRED-CREDENCIADORA TO  LD1-REL2-CREDENCIADORA
           MOVE  WRK-SRED-CODIGO-TDB    TO  LD1-REL2-CODIGO-TDB

           EVALUATE  WRK-SRED-CODIGO-TDB

           WHEN  'CR'
                 MOVE  'MASTER '       TO  LD1-REL2-DESC-TDB

           WHEN  'VC'
                 MOVE  'VISA   '       TO  LD1-REL2-DESC-TDB
           END-EVALUATE.

           MOVE  WRK-SRED-AGENCNEW        TO  LD1-REL2-AGENCNEW
           MOVE  WRK-SRED-CONTANEW        TO  LD1-REL2-CONTANEW
           MOVE  WRK-SRED-DGCTANEW        TO  LD1-REL2-DGCTANEW
           MOVE  WRK-SRED-ESTABELECIMENTO TO  LD1-REL2-ESTAB

           MOVE  WRK-SRED-CNPJ-CPF(2:8)   TO  WRK-CNPJ-EDT
           MOVE  WRK-SRED-FILIAL          TO  WRK-FILIAL-EDT
           MOVE  WRK-SRED-CTRL            TO  WRK-CNPJ-CTR-EDT
           MOVE WRK-NUM-CNPJ-EDT          TO  LD1-REL2-CNPJ

           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO

           WRITE  FD-REG-RELAFIL     FROM  LINDET1-REL2-RELAFIL
           PERFORM  1150-TESTAR-FS-RELAFIL

           ADD    1                    TO  ACU-LINHAS-2
                                           ACU-GRAVA-RELAFIL.

      *----------------------------------------------------------------*
       5300-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5310-IMPRIMIR-CABECS            SECTION.
      *----------------------------------------------------------------*

           ADD   1                     TO  ACU-PAGINAS-2
           MOVE  ACU-PAGINAS-2         TO  CB1-REL1-PAG
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO

           WRITE  FD-REG-RELAFIL     FROM  CABEC1-REL1-BANDEIRA
           PERFORM  1150-TESTAR-FS-RELAFIL

           WRITE  FD-REG-RELAFIL    FROM  CABEC2-REL1-BANDEIRA
           PERFORM  1150-TESTAR-FS-RELAFIL

           WRITE  FD-REG-RELAFIL    FROM  CABEC3-REL2-RELAFIL
           PERFORM  1150-TESTAR-FS-RELAFIL

           WRITE  FD-REG-RELAFIL    FROM  CABEC7-REL1-LINBRANCO
           PERFORM  1150-TESTAR-FS-RELAFIL

           WRITE  FD-REG-RELAFIL    FROM  CABEC4-REL1-BANDEIRA
           PERFORM  1150-TESTAR-FS-RELAFIL

           WRITE  FD-REG-RELAFIL    FROM  CABEC5-REL2-RELAFIL
           PERFORM  1150-TESTAR-FS-RELAFIL

           MOVE  6                     TO  ACU-LINHAS-2.

      *----------------------------------------------------------------*
       5310-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZA                   SECTION.
      *----------------------------------------------------------------*

           MOVE  ACU-GRAVA-RELAFIL     TO  LT1-REL1-QTD-CIELO
           MOVE  WRK-GRAVACAO          TO  WRK-OPERACAO

           WRITE  FD-REG-RELAFIL     FROM  CABEC7-REL1-LINBRANCO
           PERFORM  1150-TESTAR-FS-RELAFIL

           WRITE  FD-REG-RELAFIL     FROM  LINTOT1-REL1-BANDEIRA
           PERFORM  1150-TESTAR-FS-RELAFIL.

           DISPLAY '****************** BVVE0542 ******************'
           DISPLAY '*                                            *'
           DISPLAY '*            FIM DE PROCESSAMENTO            *'
           DISPLAY '*                                            *'
           DISPLAY '*                                            *'
           MOVE  ACU-LIDOS-TDBCOMTR    TO  WRK-MASC
           DISPLAY '*  TOTAL LIDOS TDBCOMTR       = ' WRK-MASC
                                                              '  *'
           MOVE  ACU-LIDOS-EREDEAFI    TO  WRK-MASC
           DISPLAY '*  TOTAL LIDOS EREDEAFI       = ' WRK-MASC
                                                              '  *'
           MOVE  ACU-LIDOS-BANDEIRA    TO  WRK-MASC
           DISPLAY '*  TOTAL LIDOS     ANDEIRA    = ' WRK-MASC
                                                              '  *'
           DISPLAY '*                                            *'
           DISPLAY '*                                            *'
           MOVE  ACU-GRAVA-SREDETDB    TO  WRK-MASC
           DISPLAY '*  TOTAL GRAVADOS SREDETDB    = ' WRK-MASC
                                                              '  *'
           MOVE  ACU-GRAVA-RELAFIL     TO  WRK-MASC
           DISPLAY '*  TOTAL GRAVADOS RELAFIL     = ' WRK-MASC
                                                              '  *'
           DISPLAY '*                                            *'
           DISPLAY '*--------------------------------------------*'
           DISPLAY '*                                            *'
           MOVE  ACU-QTD-COM-AFILIACAO TO  WRK-MASC
           DISPLAY '*  TOTAL COM AFILIACAO        = ' WRK-MASC
                                                              '  *'
           MOVE  ACU-QTD-SEM-AFILIACAO TO  WRK-MASC
           DISPLAY '*  TOTAL SEM AFILIACAO        = ' WRK-MASC
                                                              '  *'
           DISPLAY '*                                            *'
           DISPLAY '****************** BVVE0542 ******************'

           CLOSE  TDBCOMTR
                  EREDEAFI
                  SREDETDB
                  BANDEIRA
                  RELAFIL.

           MOVE  WRK-FECHAMENTO          TO  WRK-OPERACAO

           PERFORM  1100-TESTAR-FILE-STATUS

           MOVE  ' FINAL DA EXECUCAO'  TO  WRK-LITERAL
           PERFORM  9998-MONTA-DISPLAY.

      *----------------------------------------------------------------*
       8000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9998-MONTA-DISPLAY              SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'          USING  WRK-DATA-HORA

           MOVE  WHEN-COMPILED         TO  WRK-DT-COMPILED
           MOVE  WRK-DT-COMPILED       TO  WRK-DS-COMPILED
           MOVE  WRK-DT-COMPILED (1:2) TO  WRK-DS-COMPILED (4:2)
           MOVE  WRK-DT-COMPILED (4:2) TO  WRK-DS-COMPILED (1:2)

           MOVE  WRK-TI-HHMMSS         TO  WRK-HORA-SISTEMA.
           MOVE  WRK-HH-SISTEMA        TO  WRK-HOR-EXECUCAO
           MOVE  WRK-MM-SISTEMA        TO  WRK-MIN-EXECUCAO
           MOVE  WRK-SS-SISTEMA        TO  WRK-SEG-EXECUCAO

           MOVE  WRK-DT-AAAAMMDD       TO  WRK-DATA-SISTEMA
           MOVE  WRK-ANO-SISTEMA       TO  WRK-AA-EXECUCAO
           MOVE  WRK-MES-SISTEMA       TO  WRK-MM-EXECUCAO
           MOVE  WRK-DIA-SISTEMA       TO  WRK-DD-EXECUCAO

           DISPLAY '*--------------------------------------------*'
           DISPLAY '| BVVE0542 - COMPILED : ' WRK-DS-COMPILED '  |'
           DISPLAY '|  ' WRK-LITERAL    ' : ' WRK-DATA-EXECUCAO
                                        '   ' WRK-HORA-EXECUCAO
                                                              '  |'
           DISPLAY '*--------------------------------------------*'.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO

           MOVE 'BVVE0542'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*
