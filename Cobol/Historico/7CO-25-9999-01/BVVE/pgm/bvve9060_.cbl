*********** Convertido de OS/VS COBOL p/ COBOL for MVS em 08/07/04 10:26
*********** VERSAO PARA TESTE DO DESE1060
      *---------------------------------------------------------------*
       IDENTIFICATION                  DIVISION.
      *---------------------------------------------------------------*
       PROGRAM-ID.                     BVVE9060.
JAPI-D*REMARKS.
JAPI-D************************** CPM SISTEMAS ************************
JAPI-D*                                                              *
JAPI-D*      PROGRAMA    : DESE1060                                  *
JAPI-D*      PROGRAMADOR : MARCELO FABIANO VILLARINHO NARDI - AMS    *
JAPI-D*      ANALISTA    : RONALD/ISABEL -   GRUPO 73                *
JAPI-D*      DATA        : JANEIRO/10/95                             *
JAPI-D**-------------------------------------------------------------*
JAPI-D*                          T A B E L A S                       *
JAPI-D*                          -------------                       *
JAPI-D*              DESOV001 - DB2PRD.V01PARM_OPER_DESC   - SELECT  *
JAPI-D*      C4 -    DESOV004 - DB2PRD.V01OPER_DESC_TITLO  - CURSOR  *
JAPI-D*      C5 -    DESOV005 - DB2PRD.V01TITLO_OPER_DESC  - CURSOR  *
JAPI-D*              OBJEV001 - DB2PRD.V02CLIENTE          - SELECT  *
JAPI-D*              MESUV007 - DB2PRD.V05DEPDC_BDSCO      - SELECT  *
JAPI-D*              CEPNV003 - DB2PRD.V01CEP              - SELECT  *
JAPI-D*                                                              *
JAPI-D* -------------------------------------------------------------*
JAPI-D*                         O B J E T I V O                      *
JAPI-D*                         ---------------                      *
JAPI-D*          GERAR MOVIMENTO DE OPERACOES REALIZADAS NO DIA      *
JAPI-D*          E CONTABILIZA OPERACOES COM DT. DESCONTO = MOVTO    *
JAPI-D*                          ( COMERCIAL )                       *
JAPI-D*************************ULTIMA ALTERACAO***********************
JAPI-D*--------------------------------------------------------------*
JAPI-D*       PROGRAMADORA: ANGELA       - CPM                       *
JAPI-D*       ANALISTA    : MILTON/ENIO  - GRP-91 RAMAL 2394         *
JAPI-D*       DATA        : SETEMBRO/1996.                           *
JAPI-D*       OBJETIVO    : INCLUIR CARTEIRA 'H'.                    *
JAPI-D****************************************************************
JAPI-D*
JAPI-D*=============== ALTERACAO DE PROGRAMA ===============*
JAPI-D*               === AMS INFORMATICA ===               *
JAPI-D*                                                     *
JAPI-D*       PROGRAMADOR : MARCOS       - AMS              *
JAPI-D*       ANALISTA    : NANCI        - AMS              *
JAPI-D*       DATA        : 15/ MAIO /1998.                 *
JAPI-D*       OBJETIVO    : PROJETO ANO 2.000               *
JAPI-D*=====================================================*
JAPI-D*=============== ULTIMA  ALTERACAO ===================*
JAPI-D*                                                     *
JAPI-D*       ANALISTA    : VIVIANE MIGUEL - AMS            *
JAPI-D*       DATA        : 12/ABRIL/2000                   *
JAPI-D*       OBJETIVO    : INCLUSAO CARTEIRA 'C' E         *
JAPI-D*                     CRIAR UM ARQUIVO DE MOVIMENTO   *
JAPI-D*                     APENAS COM CARTEIRA 'C'         *
JAPI-D*                     E UM ARQUIVO CONTENTO O NOSSO   *
JAPI-D*                     NUMERO QUE EH SEQUENCIAL.       *
JAPI-D*=====================================================*
JAPI-D*        ALTERACAO STEFANINI                                   *
JAPI-D*  PROGRAMADOR : ANDREA RUNTE                                  *
JAPI-D*  ANALISTA : ANDRE NANINI - CPM                               *
JAPI-D*  ALTERACAO : ACERTAR CONTA CONTABIL                          *
JAPI-D****************************************************************
JAPI-D*                     ALTERACAO STEFANINI                      *
JAPI-D*--------------------------------------------------------------*
JAPI-D*  PROGRAMADORA: ELIANA                                        *
JAPI-D*  ANALISTA    : SALETE MELLO                                  *
JAPI-D*  ALTERACAO   : ALTERAR CODIGO DE JUNCAO DO DEPARTAMENTO      *
JAPI-D*                (AGENCIA) - DE 4103 PARA 4130                 *
JAPI-D****************************************************************
JAPI-D*        ALTERACAO  AMS INFORMATICA                            *
JAPI-D*--------------------------------------------------------------*
JAPI-D*  DATA........: ABRIL/2001                                    *
JAPI-D*  PROGRAMADOR : CLAUDIO ROGERIO                               *
JAPI-D*  ANALISTA    : CLAUDIO ROGERIO                               *
JAPI-D*  ALTERACAO   : PASSAR A CONTABILIZAR RENDAS A APROPRIAR  E   *
JAPI-D*                RECEITA EFETIVA EM CONTAS DIFERENCIADAS POR   *
JAPI-D*                CARTEIRA E TIPO DE PESSOA                     *
JAPI-D*--------------------------------------------------------------*
JAPI-D*  RENDAS A APROPRIAR:                                         *
JAPI-D*  ===================                                         *
JAPI-D*  CARTEIRA      PESSOA       RAZAO    CONTA                   *
JAPI-D*    E - B       JURIDICA     65-31    00004 - 3               *
JAPI-D*    D           JURIDICA     65-31    00002 - 7               *
JAPI-D*    C           JURIDICA     65-31    00005 - 1               *
JAPI-D*                                                              *
JAPI-D*    A - T       FISICA       NAO EXISTE                       *
JAPI-D*    E - B       FISICA       65-32    00004 - 3               *
JAPI-D*    D           FISICA       65-32    00002 - 7               *
JAPI-D*    C           FISICA       65-32    00006 - P               *
JAPI-D*  RENDA EFETIVA:                                              *
JAPI-D*  ==============                                              *
JAPI-D*  CARTEIRA      PESSOA       RAZAO    CONTA                   *
JAPI-D*    A - T       JURIDICA     09-69    5001 - 6                *
JAPI-D*    E - B       JURIDICA     09-69    5004 - 0                *
JAPI-D*    D           JURIDICA     09-69    5002 - 4                *
JAPI-D*    C           JURIDICA     09-69    5005 - 1                *
JAPI-D*                                                              *
JAPI-D*    A - T       FISICA        NAO EXISTE                      *
JAPI-D*    E - B       FISICA       09-68    5004 - 0                *
JAPI-D*    D           FISICA       09-68    5002 - 4                *
JAPI-D*    C           FISICA       09-68    5006 - 7                *
JAPI-D*==============================================================*
JAPI-D*                       A L T E R A C A O                      *
JAPI-D*--------------------------------------------------------------*
JAPI-D*                                                              *
JAPI-D*      PROGRAMADOR : MARCELO VIEIRA DE SOUZA  -  CPM/FPOLIS    *
JAPI-D*      ANALISTA CPM: MICHELLI VENTURA         -  CPM/FPOLIS    *
JAPI-D*      ANALISTA    : PATRICIA TOSCANO         -  GP. 90        *
JAPI-D*      DATA        : 13/11/2001                                *
JAPI-D*                                                              *
JAPI-D*      OBJETIVO    : IDENTIFICAR DATA DE CONTABILIZACAO DE IOF *
JAPI-D*                    PARA DEMONSTRACAO EM MICROFICHA.          *
JAPI-D*                                                              *
JAPI-D*==============================================================*
JAPI-D*                       A L T E R A C A O                      *
JAPI-D*--------------------------------------------------------------*
JAPI-D*                                                              *
JAPI-D*      ANALISTA    : CINTHIA                  -  AMS           *
JAPI-D*      DATA        : 02/04/2002                                *
JAPI-D*                                                              *
JAPI-D*      OBJETIVO    : TRATAR A INCLUSAO DE COBRANCA DE TARIFA   *
JAPI-D*                    A TARIFA QUE JA EH COBRADA HOJE (REGISTRO)*
JAPI-D*                                                              *
JAPI-D*==============================================================*
JAPI-D*==============================================================*
JAPI-D*                       A L T E R A C A O                      *
JAPI-D*--------------------------------------------------------------*
JAPI-D*                                                              *
JAPI-D*      ANALISTA    : LOHNING                  -  AMS           *
JAPI-D*      DATA        : 24/06/2002                                *
JAPI-D*                                                              *
JAPI-D*      OBJETIVO    : INCLUSAO DE NOVOS CODIGOS DE LANCTO       *
JAPI-D*                    CONFORME O CODIGO DA CART.DE DESCONTO     *
JAPI-D*                                                              *
JAPI-D*==============================================================*
JAPI-D*==============================================================*
JAPI-D*                       A L T E R A C A O                      *
JAPI-D*--------------------------------------------------------------*
JAPI-D*                                                              *
JAPI-D*      ANALISTA    : MARINA FUJITA - POLEN                     *
JAPI-D*      DATA        : NOVEMBRO/2002                             *
JAPI-D*      OBJETIVO    : INCLUSAO DE ARQUIVOS SMOVCONE E SMVCCONE  *
JAPI-D*                    PARA CONTROLE DE UTILIZACAO DE SALDOS NO  *
JAPI-D*                    CONE.                                     *
JAPI-D*==============================================================*
JAPI-D*--------------------------------------------------------------*
JAPI-D*                       A L T E R A C A O                      *
JAPI-D****************************************************************
JAPI-D*                                                              *
JAPI-D*      PROGRAMADOR : MARCOS R. BEHAR         -  AMS            *
JAPI-D*      ANALISTA....: MARCOS ROBERTO BEHAR    -  AMS            *
JAPI-D*      DATA........: MARCO/2003                                *
JAPI-D*                                                              *
JAPI-D*      OBJETIVO....: PERMITIR DESCONTAR CHEQUES JA CUSTODIADOS *
JAPI-D*                    PROPOSTA AMS/403.                         *
JAPI-D*--------------------------------------------------------------*
JAPI-D******************************************************************
JAPI-D*                       ULTIMA   ALTERACAO                       *
JAPI-D******************************************************************
JAPI-D*                                                                *
JAPI-D*   ANALISTA    : MARCIO VENICIO SILVA - STEFANINI - PERFORMANCE *
JAPI-D*   PROGRAMADOR : MARCIO VENICIO SILVA - STEFANINI - PERFORMANCE *
JAPI-D*                                                                *
JAPI-D*   DATA        : 22/04/2004                                     *
JAPI-D*                                                                *
JAPI-D*   RO          : 682.327                                        *
JAPI-D*                                                                *
JAPI-D*   OBJETIVO    : SUBSTITUICAO DO UPDATE DA TABELA DESOV004  POR *
JAPI-D*                 GRAVACAO DE ARQUIVO SEQUENCIAL PARA  NAO GERAR *
JAPI-D*                 PRENDER AS PAGINAS DA TABELA, O UPDATE PASSARA *
JAPI-D*                 A SER REALIZADO NO PROGRAMA DESE1068 UTILIZAN- *
JAPI-D*                 DO PROCESSO DE COMMIT/RESTART.                 *
JAPI-D*                                                                *
JAPI-D******************************************************************
      *--------------------------------------------------------------*
      *                       A L T E R A C A O                      *
      ****************************************************************
      *                                                              *
      *      PROGRAMADOR : RODRIGO BARATTA         -  AMS            *
      *      ANALISTA....: RODRIGO BARATTA         -  AMS            *
      *      DATA........: ABRIL/2004                                *
      *      OBJETIVO....: DESMEMBRAR O VALOR DA TAC NOS ENCARGOS.   *
      *                    PASSAR A DEMONSTRAR NO EXTRATO DE CONTA   *
      *                    CORRENTE DO CLIENTE O VALOR DA TARIFA DE  *
      *                    CONTRATACAO, GERANDO UM NOVO CODIGO DE    *
      *                    LANCAMENTO = 845.                         *
      *                    PROPOSTA AMS/252.                         *
      *--------------------------------------------------------------*
AMS143*--------------------------------------------------------------*
AMS143*                       A L T E R A C A O                      *
AMS143****************************************************************
AMS143*                                                              *
AMS143*      PROGRAMADOR : RODRIGO BARATTA         -  AMS            *
AMS143*      ANALISTA....: RODRIGO BARATTA         -  AMS            *
AMS143*      DATA........: AGOSTO/2005                               *
AMS143*                                                              *
AMS143*      OBJETIVO....: DESMONSTRAR TAXA DE JUROS.                *
AMS143*                                                              *
AMS143*--------------------------------------------------------------*
AMS161*--------------------------------------------------------------*
AMS161*                       A L T E R A C A O                      *
AMS161*                                                              *
AMS161*      PROGRAMADOR : RODRIGO BARATTA         -  AMS            *
AMS161*      ANALISTA....: RODRIGO BARATTA         -  AMS            *
AMS161*      DATA........: NOVEMBRO/2005                             *
AMS161*                                                              *
AMS161*      OBJETIVO....: ALTERACOES EM PROCEDIMENTOS DO DESCONTO   *
AMS161*                    - PASSAR A GRAVAR O ARQUIVO OPERPOLO,     *
AMS161*                      ONDE CONTEM TODAS AS OPERACOES CUJO OS  *
AMS161*                      TITULOS ENTRA PELA DIGITACAO VIA POLO.  *
AMS161*                                                              *
      *----------------------------------------------------------------*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    PROGRAMADOR.:   GUILHERME POYER        - CPM BRAXIS - P BCO *
      *    ANALISTA CPM:   ELIZANDRO NEGRI        - CPM BRAXIS - P BCO *
      *    ANALISTA....:   SERGIO G. GARCIA       - CPM BRAXIS S/A     *
      *    DATA........:   23/09/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR NOVO ARQUIVO, CONTENDO O CODIGO DO    *
      *      PRODUTO COM DUAS POSICOES NUMERICAS.                      *
AMS161*-----------------------*
       ENVIRONMENT  DIVISION.
      *-----------------------*

       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT    IS COMMA.

       INPUT-OUTPUT         SECTION.

       FILE-CONTROL.
           SELECT MESTRE    ASSIGN  TO  UT-S-MESTRE.
           SELECT ARQVDATA  ASSIGN  TO  UT-S-ARQVDATA.
           SELECT FITA4S    ASSIGN  TO  UT-S-FITA4S.
           SELECT MOVTOS    ASSIGN  TO  UT-S-MOVTOS.
           SELECT MOVCARTC  ASSIGN  TO  UT-S-MOVCARTC.
           SELECT SMOVCONE  ASSIGN  TO  UT-S-SMOVCONE.
           SELECT SMVCCONE  ASSIGN  TO  UT-S-SMVCCONE.
           SELECT NOSNUMER  ASSIGN  TO  UT-S-NOSNUMER.
MVS        SELECT ATUDES04  ASSIGN  TO  UT-S-ATUDES04.
AMS161     SELECT SMVACONE  ASSIGN  TO  UT-S-SMVACONE.
           SELECT PRODUT02  ASSIGN  TO  UT-S-PRODUT02.

      *--------------*
       DATA DIVISION.
      *--------------*

       FILE SECTION.

       FD  MESTRE
           LABEL RECORD IS STANDARD
           BLOCK 0.
       01  REG-MESTRE                 PIC X(250).

       FD  ARQVDATA
           LABEL RECORD IS STANDARD
           BLOCK 0.
       01  REG-ARQVDATA               PIC X(120).

       FD  FITA4S
           LABEL RECORD IS STANDARD.
       01  REG-FITA4S                 PIC X(100).

       FD  MOVTOS
           RECORD CONTAINS 460 CHARACTERS
           LABEL RECORD IS STANDARD.
       01  REG-MOVTOS                 PIC X(460).

       FD  MOVCARTC
           RECORD CONTAINS 460 CHARACTERS
           LABEL RECORD IS STANDARD.
       01  REG-MOVCARTC               PIC X(460).

       FD  SMOVCONE
           RECORD CONTAINS 480 CHARACTERS
           LABEL RECORD IS STANDARD.

       COPY I#DECECH.

       FD  SMVCCONE
           LABEL RECORD IS STANDARD.

       COPY I#DECECH.

       FD  NOSNUMER
           RECORD CONTAINS 010 CHARACTERS
           LABEL RECORD IS STANDARD.
       01  REG-NONUMER                PIC X(010).

MVS    FD  ATUDES04
MVS        RECORD CONTAINS 020 CHARACTERS
MVS        LABEL RECORD IS STANDARD.
MVS    01  REG-ATUDES04            PIC  X(20).

AMS161 FD  OPERPOLO
AMS161     RECORD CONTAINS 460 CHARACTERS
AMS161     LABEL RECORD IS STANDARD.
AMS161 01  REG-OPERPOLO            PIC  X(460).

AMS161 FD  SMVACONE
AMS161     LABEL RECORD IS STANDARD.

       COPY I#DECECH.

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO COM COD. PRODUTO COM 2 POSICOES, PARA SUBS- *
      *   TITUIR O MESMO CODIGO DE 1 POSICAO DO ARQUIVO MOVTOS, NO AR- *
      *   QUIVO A SER GERADO NO PROGRAMA DESE1095 PARA ENVIO AO BVVE;  *
      *            ORG. SEQUENCIAL     -   LRECL   =   030             *
      *----------------------------------------------------------------*

       FD  PRODUT02
           RECORD CONTAINS 030 CHARACTERS
           LABEL RECORD IS STANDARD.

       01 FD-PRODUT02                  PIC  X(030).

           EJECT
      **************************
       WORKING-STORAGE  SECTION.

STF    01 FILLER.
STF       03  WRK-CIDTFD-PRODT-CATAO-NULL PIC S9(04) COMP.

       77  FILLER                      PIC X(030) VALUE
           '*** INICIO DA WORKING *** '.

       77  NULL-DANO-BASE          PIC S9(04)  COMP       VALUE ZEROS.
       77  NULL-CJUNC-DEPDC-STUDO  PIC S9(04)  COMP       VALUE ZEROS.
       77  NULL-CSEQ-STUDO         PIC S9(04)  COMP       VALUE ZEROS.

       01  FILLER.

           03  WRK-FIM                 PIC X(03)  VALUE SPACES.
           03  WRK-FITA4               PIC S9(05) VALUE +0    COMP-3.
           03  WRK-FIM-DESO5           PIC X(03)  VALUE SPACES.
           03  WRK-NUMDIAS             PIC S9(05) COMP-3 VALUE ZEROS.
           03  WRK-BATCH               PIC  X(08) VALUE 'BATCH  '.
           03  WRK-CONTA               PIC  9(15) VALUE  ZEROS.
           03  WRK-DIGITO              PIC  X(01) VALUE  SPACES.
           03  WRK-TIPO-POOL           PIC  9(02) VALUE ZEROS.

           03  WRK-VLIQ-OPER           PIC 9(13)V99  VALUE ZEROS.
           03  WRK-VJURO               PIC 9(13)V99  VALUE ZEROS.
           03  WRK-VLR-JURO            PIC 9(13)V99  VALUE ZEROS.
           03  WRK-VTARIF              PIC 9(13)V99  VALUE ZEROS.
AMS252     03  WRK-VTARIF-TAC          PIC 9(13)V99  VALUE ZEROS.
           03  WRK-VIOF                PIC 9(13)V99  VALUE ZEROS.

           03  WRK-VRIOF-GERAL         PIC 9(13)V99  VALUE ZEROS.
           03  WRK-VRIOF-AGENCIA       PIC 9(13)V99  VALUE ZEROS.
           03  WRK-VRTIT-AGENCIA       PIC 9(13)V99  VALUE ZEROS.

           03  WRK-AGENCIA-ANT         PIC 9(05)     VALUE ZEROS.

AMS143     03  WRK-PJURO-DESOV006      PIC X(01)     VALUE SPACES.

MVS        03  WRK-GRAV-ATUDES04       PIC 9(10)  VALUE ZEROS.
POLEN      03  WRK-DISPL               PIC 9(10)  VALUE ZEROS.
           03  WRK-TOTMOVTOS           PIC 9(10)  VALUE ZEROS.
           03  WRK-TOTSMOVCONE         PIC 9(10)  VALUE ZEROS.
           03  WRK-TOTSMVCCONE         PIC 9(10)  VALUE ZEROS.
AMS161     03  WRK-TOTSMVACONE         PIC 9(10)  VALUE ZEROS.
           03  WRK-TOTFITA4S           PIC 9(10)  VALUE ZEROS.
AMS161     03  WRK-TOTOPERPOLO         PIC 9(10)  VALUE ZEROS.

AMS403     03  WRK-SEUNUMERO           PIC X(15).
AMS        03  WRK-SEUNUMERO-R         REDEFINES  WRK-SEUNUMERO.
AMS            05  WRK-BORDERO         PIC X(05).
AMS            05  WRK-CONTROLE.
AMS              07  WRK-CONTROLE-N    PIC 9(09).
AMS403         05  WRK-TIPO-LOTE       PIC X(01).

           03  WRK-CHV-NSONR.
               05  WRK-CHV-JULIANA     PIC 9(05).
               05  WRK-CHV-JULIANA-R REDEFINES WRK-CHV-JULIANA.
                   07  WRK-CHV-ZZ      PIC 9(02).
                   07  WRK-CHV-DDD     PIC 9(03).
               05  WRK-CHV-CONSTAN     PIC 9(01).
               05  WRK-CHV-SEQUENC     PIC 9(05).
                                       PIC 9(11).

           03  WRK-JULIANA             PIC 9(07).
           03  WRK-JULIANA-R  REDEFINES  WRK-JULIANA.
               05  WRK-JJ-SS           PIC 9(02).
               05  WRK-JJ-AA           PIC 9(02).
               05  WRK-JJ-DDD          PIC 9(03).

           03  DATA-HORA.
               05  FILLER              PIC X(07)    VALUE SPACES.
               05  DT-AAAAMMDD         PIC 9(09)    COMP-3.
               05  FILLER              PIC X(31)    VALUE SPACES.

           03  WRK-DIA-AGE.
               05  WRK-DD-DIA-AGE      PIC 99.
               05  FILLER              PIC X(08).

           03  WRK-CHAVE-DIAGE.
               05  WRK-CHAVE-DD-DG     PIC 9(02).
               05  WRK-CHAVE-AG-DG     PIC 9(05).
                                       PIC 9(07).

           03  WRK-DOPER.
               05  DIA-DOPER           PIC 99.
               05  FILLER              PIC X.
               05  MES-DOPER           PIC 99.
               05  FILLER              PIC X.
               05  SEC-DOPER           PIC 99.
               05  ANO-DOPER           PIC 99.

           03  WRK-DOPER-DESC-TITLO.
               05  SEC-DOPER1          PIC 99.
               05  ANO-DOPER1          PIC 99.
               05  MES-DOPER1          PIC 99.
               05  DIA-DOPER1          PIC 99.
           03  WRK-DOPER-DESC-TITLO-R REDEFINES WRK-DOPER-DESC-TITLO
                                       PIC 9(08).

           03  WRK-DOPER-DESC-TITLO-IN.
               05  SEC-DOPER1-IN       PIC 99.
               05  MES-DOPER1-IN       PIC 99.
               05  DIA-DOPER1-IN       PIC 99.
           03  WRK-DOPER-DESC-TITLO-IN-R REDEFINES
               WRK-DOPER-DESC-TITLO-IN PIC 9(08).

           03  WRK-INFERIOR.
               05  DIA-INFERIOR        PIC 99.
               05  FILLER              PIC X.
               05  MES-INFERIOR        PIC 99.
               05  FILLER              PIC X.
               05  ANO-INFERIOR        PIC 9999.

           03  WRK-DT-INF.
               05  WRK-DIA-INF         PIC 99.
               05  WRK-MES-INF         PIC 99.
               05  WRK-ANO-INF         PIC 9(04).

           03  WRK-SUPERIOR.
               05  DIA-SUPERIOR        PIC 99.
               05  FILLER              PIC X.
               05  FILLER              PIC X.
               05  ANO-SUPERIOR        PIC 9999.

           03  WRK-DT-SUP.
               05  WRK-DIA-SUP         PIC 99.
               05  WRK-MES-SUP         PIC 99.
               05  WRK-ANO-SUP         PIC 9(04).

           03  WRK-DT-DESOV004.
               05  WRK-DT-004-DIA      PIC 99.
               05  FILLER              PIC X VALUE '.'.
               05  WRK-DT-004-MES      PIC 99.
               05  FILLER              PIC X VALUE '.'.
               05  WRK-DT-004-SEC      PIC 99.
               05  WRK-DT-004-ANO      PIC 99.
           03  WRK-DT-DESOV004-R REDEFINES WRK-DT-DESOV004 PIC X(10).

           03  WRK-DATA-004.
               05  WRK-DIA-004         PIC 99.
               05  WRK-MES-004         PIC 99.
               05  WRK-ANO-004         PIC 99.
           03  WRK-DATA-004-R   REDEFINES  WRK-DATA-004
                                       PIC 9(08).

           03  WRK-DT-DESOV005.
               05  WRK-DT-005-DIA      PIC 99.
               05  FILLER              PIC X.
               05  WRK-DT-005-MES      PIC 99.
               05  FILLER              PIC X.
               05  WRK-DT-005-SEC      PIC 99.
               05  WRK-DT-005-ANO      PIC 99.

           03  WRK-DATA-005.
               05  WRK-DIA-005         PIC 99.
               05  WRK-MES-005         PIC 99.
               05  WRK-SEC-005         PIC 99.
               05  WRK-ANO-005         PIC 99.
           03  WRK-DATA-005-R   REDEFINES  WRK-DATA-005
                                       PIC 9(08).

FS2511         05  WRK-CGC-CPF         PIC X(09).
FS2511         05  WRK-CFLIAL          PIC X(04).
FS2511         05  WRK-CTR-CGCPF       PIC 9(02).
           03  WRK-CGC-FLIAL-R         REDEFINES WRK-CGC-CFLIAL-CPF
FS2511                                 PIC X(15).

           03  WRK-RAZAO.
               05  WRK-GRUPO           PIC 9(02).
               05  WRK-SUBGP           PIC 9(02).
               05  FILLER              PIC X(01).
           03  WRK-RAZAO-R  REDEFINES  WRK-RAZAO  PIC 9(05).

           03  WRK-DATA-NUM-DOCTO.
               05  DIA-NUMDOCTO        PIC 99.
               05  MES-NUMDOCTO        PIC 99.
               05  ANO-NUMDOCTO        PIC 99.
           03  WRK-DATA-NUM-DOCTO-R REDEFINES WRK-DATA-NUM-DOCTO
                                       PIC 9(06).

VIVI   01  WRK-CADNNUM.
VIVI       03  FILLER         PIC X(05).

       01  WRK-X3-1                    PIC  X(003)         VALUE SPACES.
       01  FILLER                      REDEFINES           WRK-X3-1.
           05  WRK-X1-3                PIC  X(001).
           05  FILLER                  PIC  X(002).

       01  WRK-S5-AUX                  PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-S5-AUX.
           05  FILLER                  PIC  X(001).
           05  WRK-5-AUX               PIC  9(005).

       01  WRK-S7-AUX                  PIC +9(007)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-S7-AUX.
           05  FILLER                  PIC  X(001).
           05  WRK-7-AUX               PIC  9(007).

       01  WRK-S2-AUX                  PIC +9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-S2-AUX.
           05  FILLER                  PIC  X(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DO ARQUIVO PRODUT2'.
      *----------------------------------------------------------------*

       01  WRK-REG-PRODUT02.
           05  WRK-CDAGE-PR2           PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-CTAPR-PR2.
             10 WRK-NRCTA-PR2          PIC  9(007) COMP-3  VALUE ZEROS.
             10 WRK-DVCTA-PR2          PIC  X(001)         VALUE SPACES.
           05  WRK-CDCAR-PR2           PIC  X(001)         VALUE SPACES.
           05  WRK-NOSNUMER-PR2        PIC  9(011) COMP-3  VALUE ZEROS.
           05  WRK-PRODUT02-PR2        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(013)         VALUE SPACES.

AMS*******--------------------------------------------------------******
AMS******* CAMPOS UTILIZADOS PARA ALTERACAO DA CONTABILIZACAO     ******
AMS******* POR TIPO DE PESSOA E TIPO DE CARTEIRA                  ******
AMS*******--------------------------------------------------------******
AMS    01     WRK-CAMPOS.
JAPI-U     03 WRK-PESSOA                    PIC X(001)  VALUE  SPACES.
JAPI-U     03 WRK-CARTEIRA                  PIC X(001)  VALUE  SPACES.
AMS****==>     DATA INICIO DA ALTERACAO
JAPI-U     03 WRK-DATA-CORTE           PIC 9(008)  VALUE  20010820.
AMS****==>     ACUMULADORES RENDAS A APROPRIAR
AMS        03 ACU-RENDAS-PESSOA-CARTEIRA.
AMS          05 ACU-RENDAS-J-CARTEIRA-A-T   PIC 9(15)V99 COMP-3 VALUE 0.
AMS          05 ACU-RENDAS-J-CARTEIRA-B-E   PIC 9(15)V99 COMP-3 VALUE 0.
AMS          05 ACU-RENDAS-F-CARTEIRA-B-E   PIC 9(15)V99 COMP-3 VALUE 0.
AMS          05 ACU-RENDAS-J-CARTEIRA-D     PIC 9(15)V99 COMP-3 VALUE 0.
AMS          05 ACU-RENDAS-F-CARTEIRA-D     PIC 9(15)V99 COMP-3 VALUE 0.
AMS          05 ACU-RENDAS-J-CARTEIRA-C     PIC 9(15)V99 COMP-3 VALUE 0.
AMS          05 ACU-RENDAS-F-CARTEIRA-C     PIC 9(15)V99 COMP-3 VALUE 0.

MVS   *---------------------------------------------------------------*
MVS   * AREA UTILIZADA PARA GRAVACAO DO ARQUIVO ATUDES04
MVS   *---------------------------------------------------------------*
MVS
MVS    01  WRK-ATUDES04.
AMS143     03 SAI-PJURO-DESOV006   PIC  X(01)  VALUE SPACES.
MVS        03 FILLER               PIC  X(14).

      *---------------------------------------------------------------*
      *                 AREAS UTILIZADAS PELA POOL1210
      *---------------------------------------------------------------*

       01  WRK-AREA-POOL1210.
           03  WRK-DT-GREG             PIC 9(08) COMP-3 VALUE ZEROS.
           03  WRK-DT-JULI             PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-MENSAGEM            PIC X(50)        VALUE SPACES.

       01  WRK-ERRO-POOL.
           03  FILLER                  PIC X(13) VALUE SPACES.
           03  FILLER                  PIC X(42) VALUE
               'ERRO NO ACESSO A POOL1210 - RETURN-CODE = '.
           03  WRK-RETURN              PIC 9(02) VALUE ZEROS.


      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

       COPY I#DECE20.

      *---------------------------------------------------------------*
      *        AREA    DA  POOL7100
      *---------------------------------------------------------------*
       COPY POL7100C.

      *---------------------------------------------------------------*
      *        BOOKS - ARQVDATA
      *---------------------------------------------------------------*
       COPY I#DECE02.

           EJECT
      *---------------------------------------------------------------*
      *        BOOKS - FITA4S    E    MOVTOS
      *---------------------------------------------------------------*
       COPY I#CCFCBN.
       COPY I#DESEFD.
           EJECT
      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V01ATVDD-DESC
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE DESOV003
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V01OPER_DESC_TITLO
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE DESOV004
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V01TITLO_OPER_DESC
      *---------------------------------------------------------------*
           EXEC SQL
                  INCLUDE DESOV005
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V01ENCAR_DESC
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE DESOV006
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V01OPER_TX_ESPCL
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE DESOV010
           END-EXEC.

      *        DEFINICAO DA TABELA DB2PRD.V012CLIENTE
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE OBJEV001
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V05DEPDC_BDSCO
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE MESUV007
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V01CEP
      *---------------------------------------------------------------*

           EXEC SQL
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DA TABELA DB2PRD.V01PARM_OPER_DESC
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE DESOV001
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DO SQLCA
      *---------------------------------------------------------------*

           EXEC SQL
                  INCLUDE SQLCA
           END-EXEC.


      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           EXEC SQL
              DECLARE   C1   CURSOR    FOR
              SELECT
                     CCART,
                     CTPO_PSSOA,
                     CMODLD_ATULZ,
                     CFUNC_BDSCO,
                     HATULZ,
                     CTPO_GARNT
              FROM DB2PRD.V01PARM_OPER_DESC
              WHERE
                   CCART              =  :DESOV001.CCART
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DE CURSOR   -   C4
      *---------------------------------------------------------------*

              DECLARE   C4   CURSOR    FOR
              SELECT
                     COPER_DESC_TITLO,
                     DOPER_DESC_TITLO,
                     PNEGOC_OPER_DESC,
                     DINIC_OPER_DESC,
                     DFNAL_OPER_DESC,
                     QTITLO_OPER_DESC,
                     VSOMA_TITLO_DESC,
                     DANO_AUTRZ,
                     CSEQ_NVEL_AUTRZ,
                     CSIT_LIM_SUPE,
                     CSIT_TX_ESPCL,
                     CSIT_OPER_DESC,
                     CFUNC_BDSCO,
                     HATULZ,
                     CJUNC_DEPDC,
                     CCTA_CORR,
                     CCART,
                     CATVDD_OPER_DESC,
                     CID_PSSOA_DESC,
                     CCTBIL_OPER_DESC,
                     DANO_BASE,
                     CJUNC_DEPDC_STUDO,
                     CSEQ_STUDO
                FROM   DB2PRD.V01OPER_DESC_TITLO
              WHERE
                    CCART             <> 'N'                      AND
                    CSIT_OPER_DESC    =  2
              ORDER BY CJUNC_DEPDC, COPER_DESC_TITLO
           END-EXEC.

      *---------------------------------------------------------------*
      *        DEFINICAO DE CURSOR  -  C5
      *---------------------------------------------------------------*

           EXEC SQL
              DECLARE   C5   CURSOR    FOR
              SELECT
                     COPER_DESC_TITLO,
                     DEMIS_TITLO_DESC,
                     DVCTO_TITLO_DESC,
                     VBRUTO_TITLO_DESC,
                     CSEU_NRO,
                     CSIT_TITLO_DESC,
                     PDESC_INDCD_ECONM,
                     IPSSOA,
                     ELOGDR_NRO_COMPL,
                     CCGC_CPF,
                     CFLIAL_CGC,
                     CCTRL_CPF_CGC,
                     CTITLO_INFMD_SCTRO,
                     CFUNC_BDSCO,
                     HATULZ,
                     CCEP,
                     CCEP_COMPL,
                     CJUNC_DEPDC,
                     CCTA_CORR,
                     CIDTFD_INDCD_TARIF,
                     CIDTFD_INDCD_JURO,
                     CIDTFD_INDCD_MORA,
                     CCART,
                     CPRZ_CART_DESC,
STF                  CIDTFD_PRODT_CATAO
              FROM DB2PRD.V01TITLO_OPER_DESC
              WHERE
                  (COPER_DESC_TITLO = :DESOV005.COPER-DESC-TITLO)  AND
                  (CSIT_TITLO_DESC  = 2)
           END-EXEC.

       01  FILLER                       PIC X(30) VALUE
           '*****  FIM  DA  WORKING  *****'.

           EJECT
      *--------------------*
       PROCEDURE DIVISION.
      *--------------------*

JAPI-U     CALL   'BRAD1050'.

       00000-PRINCIPAL                 SECTION.
      *---------------------------------------------------------------*

JAPI-U     CALL  'BRAD7600'   USING  DATA-HORA

           OPEN      INPUT  MESTRE
                            ARQVDATA
                     OUTPUT FITA4S
                            MOVTOS
                            MOVCARTC
                            SMOVCONE
                            SMVCCONE
                            NOSNUMER
MVS                         ATUDES04
AMS161                      OPERPOLO
AMS161                      SMVACONE
                            PRODUT02

           READ ARQVDATA INTO DATA-MOVIMENTO-E AT END
                DISPLAY 'DESE1060 - ARQUIVO VAZIO (ARQVDATA)'
                PERFORM 31000-ROTINA-ERRO.

           DISPLAY 'DESE1060 - MOVIMENTO PROCESSADO -' DATA-NORMAL-1E

           MOVE  DATA-NORMAL-1E                     TO  WRK-DATA-004-R.
           MOVE  WRK-DIA-004                        TO  WRK-DT-004-DIA.
           MOVE  WRK-MES-004                        TO  WRK-DT-004-MES.
           MOVE  WRK-SEC-004                        TO  WRK-DT-004-SEC.
           MOVE  WRK-ANO-004                        TO  WRK-DT-004-ANO.

           MOVE  9                                  TO  WRK-CHV-CONSTAN
           MOVE  ZEROS                              TO  WRK-CHV-SEQUENC
                                                        MST-AGENCIA.
           PERFORM   24000-OPEN-CURSOR-C4
           PERFORM   21000-FETCH-CURSOR-C4.

           IF        WRK-FIM     EQUAL 'FIM'
                     GO   TO     FINALIZAR.

           MOVE      CJUNC-DEPDC OF DESOV004 TO WRK-AGENCIA-ANT
           PERFORM   10000-PROCESSA-C4   UNTIL   WRK-FIM    =  'FIM'
           PERFORM   25000-CLOSE-CURSOR-C4

           IF  WRK-FITA4 GREATER +0
               PERFORM    19000-FITA4S-AGENCIA
               PERFORM    20000-FITA4S-GERAL.

VIVI       MOVE   WRK-CHV-NSONR-R      TO     NOSNUM.
VIVI       WRITE  REG-NONUMER          FROM   WRK-CADNNUM.

       FINALIZAR.

           DISPLAY '**************DESE1060***************'
           DISPLAY '*    TOTAL DE REGISTROS GRAVADOS    *'
           DISPLAY '*    ---------------------------    *'
MVS        DISPLAY '* ATUDES04 --->  '  WRK-GRAV-ATUDES04
           DISPLAY '* MOVTOS   --->  '  WRK-TOTMOVTOS
           DISPLAY '* MOVCARTC --->  '  WRK-TOTMOVCARTC
           DISPLAY '* SMOVCONE --->  '  WRK-TOTSMOVCONE
           DISPLAY '* SMVCCONE --->  '  WRK-TOTSMVCCONE
AMS161     DISPLAY '* OPERPOLO --->  '  WRK-TOTOPERPOLO
AMS161     DISPLAY '* SMVACONE --->  '  WRK-TOTSMVACONE
           DISPLAY '*************************************'

           MOVE     ZEROS  TO  RETURN-CODE
           CLOSE    FITA4S
                    MOVTOS
                    MESTRE
                    ARQVDATA
                    MOVCARTC
                    SMOVCONE
                    SMVCCONE
                    NOSNUMER
MVS                 ATUDES04
AMS161              OPERPOLO
AMS161              SMVACONE
                    PRODUT02

MVS********EXEC SQL COMMIT  END-EXEC.


      *---------------------------------------------------------------*
       00000-EXIT.                     EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       10000-PROCESSA-C4               SECTION.
      *---------------------------------------------------------------*

           IF  CJUNC-DEPDC OF DESOV004 NOT EQUAL WRK-AGENCIA-ANT
               PERFORM    19000-FITA4S-AGENCIA
               MOVE ZEROS                   TO WRK-VRTIT-AGENCIA
                                               WRK-VRIOF-AGENCIA
               MOVE CJUNC-DEPDC OF DESOV004 TO WRK-AGENCIA-ANT.

           MOVE COPER-DESC-TITLO OF DESOV004 TO
                COPER-DESC-TITLO OF DESOV005
                COPER-DESC-TITLO OF DESOV006.

           PERFORM   13000-ACESSAR-TABELAS.
           MOVE DOPER-DESC-TITLO OF DESOV004 TO WRK-DOPER.
           MOVE DIA-DOPER                    TO DIA-DOPER1-IN.
           MOVE MES-DOPER                    TO MES-DOPER1-IN.
           MOVE SEC-DOPER                    TO SEC-DOPER1-IN.
           MOVE ANO-DOPER                    TO ANO-DOPER1-IN.

           DISPLAY 'DATA-INVERT-1E = ' DATA-INVERT-1E
           DISPLAY 'DOPER-DESC-TITLO = ' WRK-DOPER-DESC-TITLO-IN-R
           DISPLAY 'OPER-DESC       = ' CCTBIL-OPER-DESC OF DESOV004

           IF (WRK-DOPER-DESC-TITLO-IN-R LESS DATA-INVERT-1E) AND
              (CCTBIL-OPER-DESC OF DESOV004 EQUAL 2)
              PERFORM  10100-PROCESSA-OPER-RETROATIVA
           ELSE
               IF (WRK-DOPER-DESC-TITLO-IN-R EQUAL DATA-INVERT-1E) AND
                  (CCTBIL-OPER-DESC OF DESOV004 EQUAL 1)
                  PERFORM  10110-PROCESSA-OPER-MOVTO
                  PERFORM  32000-UPDATE-DESOV004.

           PERFORM        21000-FETCH-CURSOR-C4.
      *---------------------------------------------------------------*
       10000-EXIT.        EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       10100-PROCESSA-OPER-RETROATIVA  SECTION.
      *---------------------------------------------------------------*

           PERFORM    26000-OPEN-CURSOR-C5.
           PERFORM    22000-FETCH-CURSOR-C5.

           PERFORM  11100-PROCESSA-C5-RETROATIV
                     UNTIL WRK-FIM-DESO5 = 'FIM'.

           MOVE       SPACES   TO   WRK-FIM-DESO5.

           PERFORM    27000-CLOSE-CURSOR-C5.

      *---------------------------------------------------------------*
       10100-EXIT.        EXIT.
           EJECT
      *---------------------------------------------------------------*
       10110-PROCESSA-OPER-MOVTO       SECTION.
      *---------------------------------------------------------------*

           PERFORM    26000-OPEN-CURSOR-C5.
           PERFORM    22000-FETCH-CURSOR-C5.

           PERFORM  11000-PROCESSA-C5
                    UNTIL WRK-FIM-DESO5 = 'FIM'.

           MOVE       SPACES   TO   WRK-FIM-DESO5.

           PERFORM    27000-CLOSE-CURSOR-C5.

AMS        COMPUTE WRK-VLR-JURO = ACU-RENDAS-J-CARTEIRA-A-T  +
AMS                               ACU-RENDAS-J-CARTEIRA-B-E  +
AMS                               ACU-RENDAS-J-CARTEIRA-D    +
AMS                               ACU-RENDAS-J-CARTEIRA-C    +
AMS                               ACU-RENDAS-F-CARTEIRA-B-E  +
AMS                               ACU-RENDAS-F-CARTEIRA-C.

           COMPUTE WRK-VLIQ-OPER = VSOMA-TITLO-DESC OF DESOV004 -
                   (WRK-VLR-JURO + WRK-VJURO + WRK-VIOF + WRK-VTARIF)
AMS252*             WRK-VTARIF-TAC)

           ADD +1        TO WRK-FITA4

           PERFORM   18000-FITA4S-OPERACAO

           PERFORM   18100-FITA4S-CREDITO.

           MOVE   ZEROS     TO WRK-VTARIF
AMS252                         WRK-VTARIF-TAC
                               WRK-VJURO
                               WRK-VIOF.
AMS   *==> ZERA ACUMULADORES
AMS        MOVE  ZEROS      TO ACU-RENDAS-J-CARTEIRA-A-T
AMS                            ACU-RENDAS-J-CARTEIRA-B-E
AMS                            ACU-RENDAS-J-CARTEIRA-D
AMS                            ACU-RENDAS-F-CARTEIRA-B-E
AMS                            ACU-RENDAS-F-CARTEIRA-D
AMS                            ACU-RENDAS-F-CARTEIRA-C
AMS                            WRK-VLR-JURO.

      *---------------------------------------------------------------*
       10110-EXIT.        EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       11000-PROCESSA-C5               SECTION.
      *---------------------------------------------------------------*

           PERFORM  12000-MONTAR-MOVTOS

           COMPUTE WRK-VRIOF-AGENCIA = WRK-VRIOF-AGENCIA +
                                       VIOF-TITLO-DESC OF DESOV006

           COMPUTE WRK-VRIOF-GERAL = WRK-VRIOF-GERAL +
                                     VIOF-TITLO-DESC OF DESOV006
      * - ACUMULA ENCARGO TITULO

           ADD  VTARIF-TITLO-DESC     OF DESOV006  TO WRK-VTARIF.
AMS252     ADD  VTARIF-TAC-TITLO      OF DESOV006  TO WRK-VTARIF-TAC.
           ADD  VIOF-TITLO-DESC       OF DESOV006  TO WRK-VIOF.

           IF   WRK-DOPER-DESC-TITLO-IN-R  LESS   WRK-DATA-CORTE
                ADD  VJURO-TITLO-DESC  OF DESOV006  TO WRK-VJURO
           ELSE
             IF     WRK-PESSOA EQUAL 'F'
                    PERFORM  11001-ACU-PESSOA-FISICA
             ELSE
                    PERFORM  11002-ACU-PESSOA-JURIDICA.

           PERFORM  22000-FETCH-CURSOR-C5.

      *---------------------------------------------------------------*
       11000-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
       11001-ACU-PESSOA-FISICA         SECTION.
      *---------------------------------------------------------------*

           IF   WRK-CARTEIRA   EQUAL  'B'  OR  'E'
                COMPUTE ACU-RENDAS-F-CARTEIRA-B-E    =
                        ACU-RENDAS-F-CARTEIRA-B-E    +
                        VJURO-TITLO-DESC  OF DESOV006
           ELSE
             IF   WRK-CARTEIRA   EQUAL  'D'
                  COMPUTE ACU-RENDAS-F-CARTEIRA-D      =
                          ACU-RENDAS-F-CARTEIRA-D      +
                          VJURO-TITLO-DESC  OF DESOV006
             ELSE
               IF   WRK-CARTEIRA   EQUAL  'C'
                    COMPUTE ACU-RENDAS-F-CARTEIRA-C      =
                            ACU-RENDAS-F-CARTEIRA-C      +
                            VJURO-TITLO-DESC  OF DESOV006.

      *---------------------------------------------------------------*
       11001-EXIT.                   EXIT.
           EJECT
      *---------------------------------------------------------------*
       11002-ACU-PESSOA-JURIDICA       SECTION.
      *---------------------------------------------------------------*

           IF   WRK-CARTEIRA   EQUAL  'A'  OR  'T'
                COMPUTE ACU-RENDAS-J-CARTEIRA-A-T    =
                        ACU-RENDAS-J-CARTEIRA-A-T    +
                        VJURO-TITLO-DESC  OF DESOV006
           ELSE
             IF   WRK-CARTEIRA   EQUAL  'B'  OR  'E'
                  COMPUTE ACU-RENDAS-J-CARTEIRA-B-E    =
                          ACU-RENDAS-J-CARTEIRA-B-E    +
                          VJURO-TITLO-DESC  OF DESOV006
             ELSE
               IF   WRK-CARTEIRA   EQUAL  'D'
                    COMPUTE ACU-RENDAS-J-CARTEIRA-D      =
                            ACU-RENDAS-J-CARTEIRA-D      +
                            VJURO-TITLO-DESC  OF DESOV006
               ELSE
                      COMPUTE ACU-RENDAS-J-CARTEIRA-C      =
                              ACU-RENDAS-J-CARTEIRA-C      +
                              VJURO-TITLO-DESC  OF DESOV006.

      *---------------------------------------------------------------*
       11002-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       11100-PROCESSA-C5-RETROATIV     SECTION.
      *---------------------------------------------------------------*

           PERFORM  12000-MONTAR-MOVTOS

           IF  WRK-FIM-DESO5  EQUAL  'FIM'
               GO  TO  11100-EXIT.

           PERFORM  22000-FETCH-CURSOR-C5.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       12000-MONTAR-MOVTOS             SECTION.
      *---------------------------------------------------------------*

           DISPLAY '12000-MONTAR-MOVTOS'

           IF  CCEP OF DESOV005 EQUAL ZEROS
               MOVE SPACES         TO CSGL-UF      OF CEPNV003
                                      IMUN         OF CEPNV003
               GO                  TO 12000-CONTINUA.

      * - ACESSANDO CEP

           EXEC SQL
              SELECT
                   CSGL_UF,
                   IMUN
              INTO
                   :CEPNV003.IMUN
              FROM   DB2PRD.V01CEP
              WHERE  (CCEP        = :DESOV005.CCEP)       AND
                     (CCEP_COMPL  = :DESOV005.CCEP-COMPL)
           END-EXEC.
           IF ( SQLCODE NOT EQUAL ZEROS  AND  +100)  OR
              ( SQLWARN0       =    'W'  )
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01CEP            '     TO ERR-DBD-TAB
                MOVE 'SELECT'          TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0001              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

           IF  SQLCODE    =   +100
               MOVE SPACES         TO CSGL-UF      OF CEPNV003
                                      IMUN         OF CEPNV003.

       12000-CONTINUA.
      * - ACESSANDO ENCARGOS

           EXEC SQL
              SELECT
                   COPER_DESC_TITLO,
                   CDIFDR_TITLO_DESC,
                   VTARIF_TITLO_DESC,
AMS45              VTARIF_TAC_TITLO,
                   VJURO_TITLO_DESC,
                   VIOF_TITLO_DESC,
                   VMORA_TITLO_DESC,
                   PJURO
              INTO
                   :DESOV006.COPER-DESC-TITLO,
                   :DESOV006.CDIFDR-TITLO-DESC,
                   :DESOV006.VTARIF-TITLO-DESC,
AMS45              :DESOV006.VTARIF-TAC-TITLO,
                   :DESOV006.VJURO-TITLO-DESC,
                   :DESOV006.VIOF-TITLO-DESC,
                   :DESOV006.VMORA-TITLO-DESC,
              FROM  DB2PRD.V01ENCAR_DESC
              WHERE
                  (COPER_DESC_TITLO  = :DESOV005.COPER-DESC-TITLO) AND
                  (CDIFDR_TITLO_DESC = :DESOV005.CDIFDR-TITLO-DESC)
           END-EXEC.

           IF ( SQLCODE   NOT  =    ZEROS  AND  +100 )   OR
              ( SQLWARN0       =    'W'   )
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01ENCAR_DESC     '     TO ERR-DBD-TAB
                MOVE 'SELECT'          TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0002              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

           IF  SQLCODE    =   +100
               DISPLAY 'NAO ENCONTRADO NO DESOV005'
               MOVE  'FIM'  TO  WRK-FIM-DESO5
               GO  TO  12000-EXIT.
      * - ACESSANDO RAMO DE ATIVIDADE

           MOVE   CATVDD-OPER-DESC  OF  DESOV004  TO
                  CATVDD-OPER-DESC  OF  DESOV003

           EXEC SQL  SELECT
                     CATVDD_OPER_DESC,
                     IATVDD_OPER_DESC,
                     CSIT_ISENC_IOF,
                     CFUNC_BDSCO,
                     HATULZ
                INTO
                    :DESOV003.CATVDD-OPER-DESC,
                    :DESOV003.IATVDD-OPER-DESC,
                    :DESOV003.CSIT-ISENC-IOF,
                    :DESOV003.CFUNC-BDSCO,
                    :DESOV003.HATULZ
                FROM   DB2PRD.V01ATVDD_DESC
                WHERE  CATVDD_OPER_DESC = :DESOV003.CATVDD-OPER-DESC
           END-EXEC.
           MOVE  CJUNC-DEPDC  OF  DESOV004          TO  MOVC-CDAGE-S
                                                        WRK-S5-AUX

           MOVE  CCTA-CORR    OF  DESOV004          TO  MOVC-NRCTA-S
                                                        WRK-S7-AUX
                                                        WRK-CONTA
           MOVE WRK-5-AUX                           TO  WRK-CDAGE-PR2
           MOVE WRK-7-AUX                           TO  WRK-NRCTA-PR2

JAPI-U     CALL   'BRAD0430'  USING  WRK-CONTA      WRK-DIGITO

           MOVE  WRK-DIGITO                         TO  MOVC-DVCTA-S
                                                        WRK-DVCTA-PR2

           MOVE  CCART        OF  DESOV004          TO  MOVC-CDCAR-S
                                                        WRK-X3-1
                                                        WRK-CARTEIRA
           MOVE WRK-X1-3                            TO  WRK-CDCAR-PR2

           MOVE  DATA-JULIANA-1E                    TO  WRK-JULIANA
           MOVE  WRK-JJ-DDD                         TO  WRK-CHV-DDD

VIVI       IF    MOVC-CDCAR-S    EQUAL  'C'
VIVI             MOVE  ZEROS                        TO  MOVC-NSONR-S
                                                        WRK-NOSNUMER-PR2
VIVI       ELSE
VIVI             ADD   1                            TO  WRK-CHV-SEQUENC
VIVI             MOVE  WRK-CHV-NSONR-R              TO  MOVC-NSONR-S
                                                       WRK-NOSNUMER-PR2.

           MOVE  ZEROS                              TO  MOVC-PXDEP-S
           MOVE  ZEROS                              TO  MOVC-AGDEP-S

AMS403     IF    MOVC-CDCAR-S    EQUAL  'C'
AMS403           MOVE  CSEU-NRO  OF  DESOV005       TO  WRK-SEUNUMERO
AMS403           IF   (WRK-CONTROLE              EQUAL SPACES) OR
AMS403                (WRK-CONTROLE              NOT NUMERIC)
AMS403                 MOVE ZEROS                   TO  WRK-CONTROLE
AMS403                 MOVE WRK-CONTROLE-N          TO  MOVC-CONTROLE
AMS403           ELSE

AMS403     IF    MOVC-CDCAR-S    EQUAL  'C'
AMS403           IF    WRK-TIPO-LOTE     NOT  EQUAL       'C'
AMS403                 MOVE  ZEROS                  TO  WRK-CONTROLE
AMS403                 MOVE  WRK-SEUNUMERO          TO  MOVC-SEUNR-S
AMS403           ELSE
AMS403                 MOVE  WRK-SEUNUMERO          TO  MOVC-SEUNR-S
AMS403     ELSE
                 MOVE  CSEU-NRO    OF  DESOV005     TO  MOVC-SEUNR-S.

           MOVE  VBRUTO-TITLO-DESC OF  DESOV005     TO  MOVC-VRTIT-S
           MOVE ZEROS                               TO  MOVC-AAAUT-S
                                                        MOVC-CDAUT-S

           MOVE  DVCTO-TITLO-DESC  OF  DESOV005     TO  WRK-DT-DESOV005
           MOVE  WRK-DT-005-DIA                     TO  WRK-DIA-005
           MOVE  WRK-DT-005-MES                     TO  WRK-MES-005
           MOVE  WRK-DT-005-SEC                     TO  WRK-SEC-005
           MOVE  WRK-DT-005-ANO                     TO  WRK-ANO-005
           MOVE  WRK-DATA-005-R                     TO  MOVC-DTVEN-S

           MOVE  DOPER-DESC-TITLO  OF  DESOV004     TO  WRK-DT-DESOV004
           MOVE  WRK-DT-004-DIA                     TO  WRK-DIA-004
           MOVE  WRK-DT-004-MES                     TO  WRK-MES-004
           MOVE  WRK-DT-004-SEC                     TO  WRK-SEC-004
           MOVE  WRK-DT-004-ANO                     TO  WRK-ANO-004
           MOVE  WRK-DATA-004-R                     TO  MOVC-DTDES-S

           IF    CTPO-GARNT  OF  DESOV001 =  001
                 MOVE 'D'                           TO  MOVC-DUPNP-S
           ELSE
                 MOVE 'P'                           TO  MOVC-DUPNP-S.

           MOVE  CID-PSSOA-DESC    OF  DESOV004     TO  MOVC-COMIS-S.

           IF   CTPO-CTA-CORR  OF  OBJEV001  =  '00'
                MOVE 'F'                            TO  MOVC-PESOA-S
                                                        WRK-PESSOA
           ELSE
                MOVE 'J'                            TO  MOVC-PESOA-S

STF        IF WRK-CIDTFD-PRODT-CATAO-NULL EQUAL -1
STF           MOVE ZEROS TO MOVC-PRODUTO-S
                            WRK-PRODUT02-PR2
STF        ELSE
STF           MOVE CIDTFD-PRODT-CATAO OF DESOV005 TO MOVC-PRODUTO-S
                                                     WRK-S2-AUX
              MOVE WRK-2-AUX                      TO WRK-PRODUT02-PR2
STF        END-IF

           MOVE  IPSSOA            OF  DESOV005     TO  MOVC-NMSAC-S
           MOVE  ELOGDR-NRO-COMPL  OF  DESOV005     TO  MOVC-ENSAC-S
           MOVE  '2'                                TO  MOVC-CENTR-S
           MOVE  IDEPDC-BDSCO      OF  MESUV007     TO  MOVC-NMAGC-S
           MOVE  SPACES                             TO  MOVC-NMAGD-S
           MOVE  ZEROS                              TO  MOVC-NRSEQ-S

           MOVE  ZEROS                              TO  MOVC-TXCOR-S

           IF    CSIT-TX-ESPCL OF  DESOV004  EQUAL  2
                 MOVE 'E'                           TO  MOVC-ESPNO-S
                 MOVE '1'                           TO  MOVC-TXESP-S
           ELSE
                 MOVE '0'                           TO  MOVC-TXESP-S
                 MOVE 'N'                           TO  MOVC-ESPNO-S
                 MOVE CID-PSSOA-DESC OF DESOV004    TO  MOVC-CMSAL-S.


           MOVE ICLI   OF   OBJEV001                TO  MOVC-NMPRO-S

           MOVE CCGC-CPF        OF OBJEV001         TO  WRK-CGC-CPF
           MOVE CFLIAL-CGC      OF OBJEV001         TO  WRK-CFLIAL
           MOVE CCTRL-CPF-CGC   OF OBJEV001         TO  WRK-CTR-CGCPF
           MOVE WRK-CGC-FLIAL-R                    TO  MOVC-CGCPR-S

           MOVE SPACES                              TO  MOVC-CORES-S
           PERFORM 12500-SUBCENTROS
           MOVE DOPER-DESC-TITLO OF DESOV004  TO WRK-INFERIOR
           MOVE DVCTO-TITLO-DESC OF DESOV005  TO WRK-SUPERIOR

AMS143        IF WRK-TIPO-LOTE     NOT  EQUAL       'C'
AMS143           IF CSIT-TX-ESPCL OF DESOV004 EQUAL  1
AMS143              MOVE  ZEROS       TO  PJURO OF DESOV006
AMS143              MOVE 'S'          TO  WRK-PJURO-DESOV006
AMS143           ELSE
AMS143              MOVE 'N'       TO  WRK-PJURO-DESOV006
AMS143        ELSE
AMS143           MOVE 'N'        TO  WRK-PJURO-DESOV006
AMS143     ELSE
AMS143        MOVE 'N'          TO  WRK-PJURO-DESOV006.


           COMPUTE MOVC-TXAPL-S = (PJURO OF DESOV006 * 12)

           MOVE 0                                   TO  MOVC-CDESA-S

           MOVE CCGC-CPF          OF DESOV005       TO  MOVC-NRCGS-S
           MOVE CFLIAL-CGC        OF DESOV005       TO  MOVC-FLCGS-S
           MOVE CCTRL-CPF-CGC     OF DESOV005       TO  MOVC-CTCGS-S
           MOVE ZEROS                               TO  MOVC-CEPXX-S
           MOVE ZEROS                               TO  MOVC-CTCA2-S
           MOVE ZEROS                               TO  MOVC-CDSEG-S
           MOVE CATVDD-OPER-DESC  OF DESOV004       TO  MOVC-CDRAM-S
                                                        MOVC-RAMOX-S
           MOVE VJURO-TITLO-DESC  OF DESOV006       TO  MOVC-VRJUR-S
           MOVE ZEROS                               TO  MOVC-VRFCA-S
           MOVE VIOF-TITLO-DESC   OF DESOV006       TO  MOVC-VRIOF-S
           IF   (WRK-NUMDIAS                   LESS  365)
                MOVE '0'                            TO  MOVC-PERIS-S
           ELSE
                MOVE '1'                            TO  MOVC-PERIS-S.
           MOVE ZEROS                               TO  MOVC-DESPP-S
           MOVE SPACES                              TO  MOVC-ENAGD-S
           MOVE ZEROS                               TO  MOVC-VRCOM-S
                                                        MOVC-VRMOD-S.
      *    MOVE VMORA-TITLO-DESC  OF DESOV006       TO  MOVC-VRMOD-S

           MOVE CFUNC-BDSCO       OF  DESOV004      TO  MOVC-CDRES-S
           MOVE CCEP              OF  DESOV005      TO  MOVC-CEPNS-S
           MOVE CCEP-COMPL        OF  DESOV005      TO  MOVC-CEPCS-S
           MOVE CRZ-CTBIL         OF  OBJEV001      TO  MOVC-CDRAZ-S

           IF   MOVC-DUPNP-S      EQUAL  'D'
                MOVE  ZEROS                         TO  MOVC-TXBOR-S
AMS45           COMPUTE MOVC-TXCOB-S = VTARIF-TITLO-DESC +
AMS45                                  VTARIF-TAC-TITLO
           ELSE
AMS45           COMPUTE MOVC-TXBOR-S = VTARIF-TITLO-DESC +
AMS45                                  VTARIF-TAC-TITLO
                MOVE  ZEROS                        TO  MOVC-TXCOB-S.

           IF   CTPO-CTA-CORR OF OBJEV001  EQUAL  '00'
                MOVE    64280                      TO  MOVC-RAZDB-S
           ELSE
                MOVE    74700                      TO  MOVC-RAZDB-S.

           MOVE IMUN           OF  CEPNV003        TO  MOVC-NMCID-S
           MOVE CSGL-UF        OF  CEPNV003        TO  MOVC-CDEST-S

VIVI       IF    MOVC-CDCAR-S    EQUAL  'C'
VIVI       ELSE
VIVI             MOVE DOPER-DESC-TITLO             TO  WRK-INFERIOR
VIVI             MOVE DVCTO-TITLO-DESC OF DESOV005 TO  WRK-SUPERIOR
VIVI             PERFORM   15000-CALCULA-DIAS
VIVI             MOVE WRK-NUMDIAS                  TO  MOVC-NRDIA-S.

           MOVE ZEROS                              TO  MOVC-VRDEF-S

           MOVE SPACES                             TO  MOVC-CDPRZ-S

           MOVE ZEROS                              TO  MOVC-VRDTD-S
                                                   MOVC-DATA-EMISSAO-S

           MOVE WRK-DOPER-DESC-TITLO-IN-R
                                       TO WRK-DT-GREG.

JAPI-U     CALL 'BRAD1210'             USING WRK-DT-GREG
                                             WRK-DT-JULI
                                             WRK-MENSAGEM.


TESTE      DISPLAY 'MOVC-CDCAR-S = ' MOVC-CDCAR-S
TESTE      DISPLAY 'CSEU-NRO     = ' CSEU-NRO OF DESOV005

VIVI       IF    MOVC-CDCAR-S    EQUAL  'C'
VIVI             PERFORM             35000-SELECT-DESOV010
VIVI             WRITE  REG-MOVCARTC FROM    REG-MOVC-S
VIVI             ADD    1            TO      WRK-TOTMOVCARTC
VIVI       ELSE
AMS161           IF (MOVC-CDCAR-S    EQUAL  'A' OR 'T')  AND
AMS161              (CSEU-NRO OF DESOV005 EQUAL 'DIGITADOVIAPOLO')
AMS161               MOVE COPER-DESC-TITLO OF DESOV004
AMS161                                   TO      MOVC-CONTROLE
AMS161               COMPUTE MOVC-TXAPL-S = (MOVC-TXAPL-S / 12)
AMS161               WRITE  REG-OPERPOLO FROM    REG-MOVC-S
AMS161               ADD    1            TO      WRK-TOTOPERPOLO
AMS161          ELSE
AMS161               MOVE COPER-DESC-TITLO OF DESOV004
AMS161                                   TO      MOVC-CONTROLE
VIVI                 WRITE  REG-MOVTOS   FROM    REG-MOVC-S
VIVI                 ADD    1            TO      WRK-TOTMOVTOS.

           IF  (DANO-BASE  OF  DESOV004  NOT EQUAL  ZEROS)
           AND (CCART      OF  DESOV004  NOT EQUAL  'N')
               IF  MOVC-CDCAR-S    EQUAL  'C'
                   MOVE  REG-MOVC-S    TO  REG-MOVT         OF SMVCCONE
                   MOVE  DANO-BASE     OF  DESOV004
                                       TO  MOVT-ANO-CONE    OF SMVCCONE
                   MOVE  CJUNC-DEPDC-STUDO OF DESOV004
                                       TO  MOVT-AGE-CONE    OF SMVCCONE
                   MOVE  CSEQ-STUDO    OF  DESOV004
                                       TO  MOVT-SEQ-CONE    OF SMVCCONE
                   MOVE  ZEROS         TO  MOVT-RAZAO       OF SMVCCONE
                   WRITE  REG-MOVT     OF  SMVCCONE
                   ADD    1            TO  WRK-TOTSMVCCONE
               ELSE
AMS161             IF (MOVC-CDCAR-S    EQUAL  'A' OR 'T')  AND
AMS161                (CSEU-NRO OF DESOV005 EQUAL 'DIGITADOVIAPOLO')
AMS161                 MOVE  REG-MOVC-S    TO  REG-MOVT      OF SMVACONE
AMS161                 MOVE  DANO-BASE     OF  DESOV004
AMS161                 MOVE  CJUNC-DEPDC-STUDO OF DESOV004
AMS161                                     TO  MOVT-AGE-CONE OF SMVACONE
AMS161                 MOVE  CSEQ-STUDO    OF  DESOV004
AMS161                                     TO  MOVT-SEQ-CONE OF SMVACONE
AMS161                 MOVE  ZEROS         TO  MOVT-RAZAO    OF SMVACONE
AMS161                 WRITE  REG-MOVT     OF  SMVACONE
AMS161                 ADD    1            TO  WRK-TOTSMVACONE
                   ELSE
                       MOVE  REG-MOVC-S    TO  REG-MOVT      OF SMOVCONE
                       MOVE  DANO-BASE     OF  DESOV004
                                           TO  MOVT-ANO-CONE OF SMOVCONE
                       MOVE  CJUNC-DEPDC-STUDO OF DESOV004
                                           TO  MOVT-AGE-CONE OF SMOVCONE
                       MOVE  CSEQ-STUDO    OF  DESOV004
                                           TO  MOVT-SEQ-CONE OF SMOVCONE
                       MOVE  ZEROS         TO  MOVT-RAZAO    OF SMOVCONE
                       WRITE  REG-MOVT     OF  SMOVCONE
                       ADD    1            TO  WRK-TOTSMOVCONE.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       12500-SUBCENTROS                SECTION.
      *---------------------------------------------------------------*

           PERFORM 12501-LE-MESTRE UNTIL
                                   MOVC-CDAGE-S EQUAL MST-AGENCIA OR
                                   MST-AGENCIA  EQUAL 99999

           IF MST-AGENCIA  EQUAL 99999
               MOVE 4499  TO MOVC-CDSUB-S
           ELSE
               IF  MST-RETRANS EQUAL ZEROS
                   MOVE 4499  TO MOVC-CDSUB-S
               ELSE
                   IF  MST-RETRANS EQUAL 5
                       MOVE 4500  TO MOVC-CDSUB-S
                   ELSE
                       IF  MST-SUBC EQUAL ZEROS
                       ELSE
                           MOVE MST-SUBC TO MOVC-CDSUB-S.

      *---------------------------------------------------------------*
       12500-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       12501-LE-MESTRE                 SECTION.
      *---------------------------------------------------------------*

           READ MESTRE INTO REG-MESTREAG AT END
                MOVE 99999 TO MST-AGENCIA.

      *---------------------------------------------------------------*
       12501-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       13000-ACESSAR-TABELAS           SECTION.

           MOVE CJUNC-DEPDC OF DESOV004  TO  CJUNC-DEPDC OF MESUV007

      * - ACESSANDO AGENCIA

           EXEC SQL
              SELECT
                    IDEPDC_BDSCO,
                    CJUNC_DEPDC_SCTRO
              INTO
                    :MESUV007.IDEPDC-BDSCO,
                    :MESUV007.CJUNC-DEPDC-SCTRO
              FROM  DB2PRD.V05DEPDC_BDSCO
              WHERE CJUNC_DEPDC = :MESUV007.CJUNC-DEPDC
           END-EXEC.

           IF ( SQLCODE   NOT  =    ZEROS)   OR
              ( SQLWARN0       =    'W'   )
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01DEPDC-BDSCO    '     TO ERR-DBD-TAB
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0003              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.


           MOVE CJUNC-DEPDC OF  DESOV004  TO
                CJUNC-DEPDC OF  OBJEV001

           MOVE CCTA-CORR   OF  DESOV004  TO
                CCTA-CORR   OF  OBJEV001

      * - ACESSANDO CLIENTE

           EXEC SQL
                SELECT
                     CJUNC_DEPDC,
                     CRZ_CTBIL,
                     CCTA_CORR,
                     ICLI,
                     CCEP,
                     CCGC_CPF,
                     CFLIAL_CGC,
                     CCTRL_CPF_CGC,
                     CRAMO_ATVDD,
                     CTPO_CTA_CORR,
                     CCEP_COMPL
                INTO
                    :OBJEV001.CJUNC-DEPDC,
                    :OBJEV001.CRZ-CTBIL,
                    :OBJEV001.CCTA-CORR,
                    :OBJEV001.ICLI,
                    :OBJEV001.ELOGDR-NRO-COMPL,
                    :OBJEV001.CCEP,
                    :OBJEV001.CCGC-CPF,
                    :OBJEV001.CFLIAL-CGC,
                    :OBJEV001.CCTRL-CPF-CGC,
                    :OBJEV001.CRAMO-ATVDD,
                    :OBJEV001.CTPO-CTA-CORR,
                    :OBJEV001.CCEP-COMPL
              WHERE
                  (CJUNC_DEPDC        =  :OBJEV001.CJUNC-DEPDC) AND
                  (CCTA_CORR          =  :OBJEV001.CCTA-CORR)
           END-EXEC.

retira     IF ( SQLCODE     =    +100  )
retira         MOVE 00       TO CTPO-CTA-CORR OF OBJEV001
retira     ELSE


           IF ( SQLCODE   NOT  =    ZEROS )   OR
              ( SQLWARN0       =    'W'   )
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V02CLIENTE        '     TO ERR-DBD-TAB
                MOVE 'SELECT'          TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0004              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

               NEXT SENTENCE
           ELSE
               MOVE 00       TO CTPO-CTA-CORR OF OBJEV001.

      * - ACESSANDO PARAMETRO

           MOVE      CCART OF DESOV004   TO CCART OF DESOV001

           PERFORM   28000-OPEN-CURSOR-C1
           PERFORM   23000-FETCH-CURSOR-C1
           PERFORM   29000-CLOSE-CURSOR-C1.

      *---------------------------------------------------------------*
       13000-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       14000-GRAVAR-REGFIM             SECTION.
      *---------------------------------------------------------------*

                                    MOVC-NRCTA-S
                                    MOVC-DVCTA-S
                                    MOVC-NSONR-S
                                    MOVC-SEUNR-S
                                    MOVC-VRTIT-S
                                    MOVC-DTVEN-S
                                    MOVC-DTDES-S
                                    MOVC-NRSEQ-S
                                    MOVC-TXAPL-S
                                    MOVC-TXCOR-S
                                    MOVC-CGCPR-S
                                    MOVC-AAAUT-S
                                    MOVC-CDAUT-S
                                    MOVC-CDSUB-S
                                    MOVC-NRCGS-S
                                    MOVC-FLCGS-S
                                    MOVC-CTCGS-S
                                    MOVC-CEPXX-S
                                    MOVC-CTCA1-S
                                    MOVC-CTCA2-S
                                    MOVC-VRJUR-S
                                    MOVC-VRFCA-S
                                    MOVC-VRIOF-S
                                    MOVC-TXCOB-S
                                    MOVC-DESPP-S
                                    MOVC-VRCOM-S
                                    MOVC-VRMOD-S
                                    MOVC-TXESP-S
                                    MOVC-CDRES-S
                                    MOVC-CEPNS-S
                                    MOVC-CEPCS-S
                                    MOVC-NRSQA-S
                                    MOVC-CDRAZ-S
                                    MOVC-TXBOR-S
                                    MOVC-RAZDB-S
                                    MOVC-NRDIA-S
                                    MOVC-VRDEF-S
                                    MOVC-VRDTD-S.

           MOVE  SPACES        TO   MOVC-DUPNP-S
                                    MOVC-CENTR-S
                                    MOVC-COMIS-S
                                    MOVC-CDPRZ-S
                                    MOVC-PESOA-S
                                    MOVC-NMSAC-S
                                    MOVC-ENSAC-S
                                    MOVC-NMAGC-S
                                    MOVC-NMAGD-S
                                    MOVC-ESPNO-S
                                    MOVC-NMPRO-S
                                    MOVC-CORES-S
                                    MOVC-CMSAL-S
                                    MOVC-CDRAM-S
                                    MOVC-PERIS-S
                                    MOVC-ENAGD-S
                                    MOVC-NMCID-S
                                    MOVC-CDEST-S.

           MOVE   DT-AAAAMMDD       TO  MOVC-DTDES-S.

           ADD    1               TO    WRK-TOTMOVTOS.

      *---------------------------------------------------------------*
       14000-EXIT.     EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       15000-CALCULA-DIAS    SECTION.
      *---------------------------------------------------------------*

           MOVE DIA-INFERIOR      TO  WRK-DIA-INF
           MOVE MES-INFERIOR      TO  WRK-MES-INF
           MOVE ANO-INFERIOR      TO  WRK-ANO-INF

           MOVE DIA-SUPERIOR      TO  WRK-DIA-SUP
           MOVE MES-SUPERIOR      TO  WRK-MES-SUP
           MOVE ANO-SUPERIOR      TO  WRK-ANO-SUP

JAPI-U     CALL 'BRAD0015' USING  WRK-DT-INF  WRK-DT-SUP  WRK-NUMDIAS.

                MOVE        15   TO   WRK-TIPO-POOL
                PERFORM     30000-ERRO-DATA.

      *----------------------------------------------*
       15000-EXIT.             EXIT.
      *----------------------------------------------*

      *---------------------------------------------------------------*
       16000-MONTAR-DIA-SUB         SECTION.
      *---------------------------------------------------------------*

           MOVE DATA-EDIT-1E                    TO   WRK-DIA-AGE
           MOVE WRK-DD-DIA-AGE                  TO   WRK-CHAVE-DD-DG
STF        MOVE 04130                           TO   WRK-CHAVE-AG-DG.

      *----------------------------------------------*
       16000-EXIT.             EXIT.
      *----------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           MOVE DATA-EDIT-1E                   TO   WRK-DIA-AGE
           MOVE WRK-DD-DIA-AGE                 TO   WRK-CHAVE-DD-DG
           MOVE WRK-AGENCIA-ANT                TO   WRK-CHAVE-AG-DG.

      *----------------------------------------------*
       17000-EXIT.             EXIT.
      *----------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       18000-FITA4S-OPERACAO           SECTION.
      *---------------------------------------------------------------*

           MOVE     DOPER-DESC-TITLO  OF  DESOV004  TO  WRK-DOPER
           MOVE     DIA-DOPER                       TO  DIA-DOPER1
           MOVE     MES-DOPER                       TO  MES-DOPER1
           MOVE     SEC-DOPER                       TO  SEC-DOPER1
           MOVE     ANO-DOPER                       TO  ANO-DOPER1


           MOVE WRK-DOPER-DESC-TITLO-R        TO  MCTS-DAT-MOV-AAAAMMDD
           MOVE CJUNC-DEPDC OF DESOV004       TO  MCTS-AGENCIA

AMS161     IF   CCART OF DESOV004 EQUAL 'A' AND
AMS161          CSEU-NRO OF DESOV005 EQUAL 'DIGITADOVIAPOLO'  AND
AMS161          CTPO-CTA-CORR OF OBJEV001  NOT EQUAL '00'
AMS161          MOVE 064                      TO  MCTS-GRUPO
AMS161          MOVE 029                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'A' AND
                CTPO-CTA-CORR OF OBJEV001  NOT EQUAL '00'
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 023                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'H' AND
                CTPO-CTA-CORR OF OBJEV001  NOT EQUAL '00'
                MOVE 003                      TO  MCTS-GRUPO
                MOVE 018                      TO  MCTS-SUBGP
           ELSE
                CTPO-CTA-CORR OF OBJEV001  EQUAL '00'
                MOVE 003                      TO  MCTS-GRUPO
                MOVE 017                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'B' AND
                CTPO-CTA-CORR OF OBJEV001  NOT EQUAL '00'
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 024                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'B' AND
                CTPO-CTA-CORR OF OBJEV001  EQUAL '00'
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 025                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'E' AND
                CTPO-CTA-CORR OF OBJEV001  NOT EQUAL '00'
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 026                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'E' AND
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 027                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'D' AND
                CTPO-CTA-CORR OF OBJEV001   NOT EQUAL '00'
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 028                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'D' AND
                CTPO-CTA-CORR OF OBJEV001   EQUAL '00'
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 029                      TO  MCTS-SUBGP
           ELSE
AMS161     IF   CCART OF DESOV004 EQUAL 'T' AND
AMS161          CSEU-NRO OF DESOV005 EQUAL 'DIGITADOVIAPOLO'  AND
AMS161          CTPO-CTA-CORR OF OBJEV001  NOT EQUAL '00'
AMS161          MOVE 064                      TO  MCTS-GRUPO
AMS161          MOVE 029                      TO  MCTS-SUBGP
           ELSE
           IF   CCART OF DESOV004 EQUAL 'T' AND
                MOVE 063                      TO  MCTS-GRUPO
                MOVE 030                      TO  MCTS-SUBGP
           ELSE
VIVI       IF   CCART OF DESOV004 EQUAL 'C'
AMS403          IF WRK-TIPO-LOTE   EQUAL     'C' AND
AMS403             CTPO-CTA-CORR OF OBJEV001  NOT EQUAL '00'
AMS403             MOVE 063                   TO  MCTS-GRUPO
AMS403             MOVE 081                   TO  MCTS-SUBGP
AMS403          ELSE
AMS403          IF WRK-TIPO-LOTE   EQUAL     'C' AND
AMS403             CTPO-CTA-CORR OF OBJEV001  EQUAL '00'
AMS403             MOVE 063                   TO  MCTS-GRUPO
AMS403             MOVE 018                   TO  MCTS-SUBGP
AMS403          ELSE
STF                MOVE 074                   TO  MCTS-GRUPO
STF                MOVE 070                   TO  MCTS-SUBGP
VIVI       ELSE
STF             MOVE 074                      TO  MCTS-GRUPO
STF             MOVE 070                      TO  MCTS-SUBGP.

           MOVE 9                             TO  MCTS-DIGITO
           MOVE 7                             TO  MCTS-LANCAMENTO
           PERFORM 17000-MONTAR-DIA-AGENCIA
           MOVE WRK-CHAVE-DIAGE-R             TO  MCTS-NUMERO-DOC
           MOVE 1                             TO  MCTS-DEB-CRED
           MOVE VSOMA-TITLO-DESC OF DESOV004  TO  MCTS-VALOR
           COMPUTE WRK-VRTIT-AGENCIA  = WRK-VRTIT-AGENCIA  +
                                        VSOMA-TITLO-DESC OF DESOV004
           PERFORM  40000-MONTA-FIXOS.

      *----------------------------------------------*
       18000-EXIT.             EXIT.
      *----------------------------------------------*

      *---------------------------------------------------------------*
       18100-FITA4S-CREDITO            SECTION.
      *---------------------------------------------------------------*

      *----( CREDITO, RAZAO CLIENTE PELO VALOR LIQUIDO DA OPERACAO )

           MOVE ZEROS                         TO  MCTS-RAZAO
           MOVE CRZ-CTBIL   OF OBJEV001       TO  WRK-RAZAO-R
           MOVE WRK-GRUPO                     TO  MCTS-GRUPO
           MOVE WRK-SUBGP                     TO  MCTS-SUBGP
           MOVE CCTA-CORR   OF DESOV004       TO  MCTS-CONTA

           MOVE   4                           TO  WRK-TAMANHO.
JAPI-U     CALL   'BRAD0431'  USING  MCTS-CONTA  WRK-DIGITO
                                                 WRK-TAMANHO.
           MOVE   WRK-DIGITO                  TO  MCTS-DIGITO

           MOVE WRK-DD-DIA-AGE                TO  WRK-CHAVE-DD-DG
           MOVE CJUNC-DEPDC      OF DESOV004  TO  WRK-CHAVE-AG-DG
           MOVE WRK-CHAVE-DIAGE-R             TO  MCTS-NUMERO-DOC

LOH   *    MOVE 7                             TO  MCTS-LANCAMENTO

LOH        IF  CCART OF DESOV004  EQUAL  'A'
LOH            MOVE 007                       TO  MCTS-LANCAMENTO
LOH        ELSE
LOH            MOVE 840                       TO  MCTS-LANCAMENTO
LOH        ELSE
LOH        IF  CCART OF DESOV004  EQUAL  'D'
LOH            MOVE 895                       TO  MCTS-LANCAMENTO
LOH        ELSE
LOH        IF  CCART OF DESOV004  EQUAL  'B'  OR  'E'
LOH            MOVE 954                       TO  MCTS-LANCAMENTO
LOH        ELSE
LOH            MOVE 007                       TO  MCTS-LANCAMENTO.
           MOVE 2                             TO  MCTS-DEB-CRED
           MOVE WRK-VLIQ-OPER                 TO  MCTS-VALOR
           PERFORM  40000-MONTA-FIXOS.

AMS252     MOVE CJUNC-DEPDC OF DESOV004       TO  MCTS-AGENCIA
AMS252     MOVE ZEROS                         TO  MCTS-RAZAO
AMS252     MOVE CRZ-CTBIL   OF OBJEV001       TO  WRK-RAZAO-R
AMS252     MOVE WRK-GRUPO                     TO  MCTS-GRUPO
AMS252     MOVE WRK-SUBGP                     TO  MCTS-SUBGP
AMS252     MOVE CCTA-CORR   OF DESOV004       TO  MCTS-CONTA
AMS252
JAPI-U     CALL   'BRAD0431'  USING  MCTS-CONTA  WRK-DIGITO
AMS252                                           WRK-TAMANHO.
AMS252     MOVE   WRK-DIGITO                  TO  MCTS-DIGITO
AMS252
AMS252     MOVE WRK-DD-DIA-AGE                TO  WRK-CHAVE-DD-DG
AMS252     MOVE CJUNC-DEPDC      OF DESOV004  TO  WRK-CHAVE-AG-DG
AMS252     MOVE WRK-CHAVE-DIAGE-R             TO  MCTS-NUMERO-DOC
AMS252     MOVE 845                           TO  MCTS-LANCAMENTO.
AMS252     MOVE 1                             TO  MCTS-DEB-CRED.
AMS252     MOVE WRK-VTARIF-TAC                TO  MCTS-VALOR.
AMS252     PERFORM  40000-MONTA-FIXOS.

      *----( CREDITO, RAZAO 65.31/32 PELO VALOR DOS JUROS )

           IF   WRK-VJURO  NOT EQUAL ZEROS
                PERFORM    18110-GRAVA-FORMA-ANTIGA.

           PERFORM    18120-GRAVA-PESSOA-CARTEIRA.

      *----( CREDITO, RAZAO 99.14/08 PELO VALOR DA TARIFA )
           IF   CTPO-GARNT OF DESOV001 EQUAL 22 OR 29 OR 25
                MOVE        099               TO  MCTS-GRUPO
                MOVE        008               TO  MCTS-SUBGP
           ELSE
                MOVE        099               TO  MCTS-GRUPO
                MOVE        014               TO  MCTS-SUBGP.

AMS403     IF   CCART OF DESOV004 EQUAL 'C'  AND
AMS403          WRK-TIPO-LOTE     EQUAL 'C'
AMS403          MOVE 3                        TO  MCTS-CONTA
AMS403          MOVE 5                        TO  MCTS-DIGITO
AMS403     ELSE
                MOVE 1                        TO  MCTS-CONTA
                MOVE 9                        TO  MCTS-DIGITO.

           MOVE 451                           TO  MCTS-LANCAMENTO
           MOVE 2                             TO  MCTS-DEB-CRED
           MOVE WRK-VTARIF                    TO  MCTS-VALOR
           PERFORM  40000-MONTA-FIXOS.

AMS252
AMS252     IF   CTPO-GARNT OF DESOV001 EQUAL 22 OR 29 OR 25
AMS252          MOVE        099               TO  MCTS-GRUPO
AMS252          MOVE        008               TO  MCTS-SUBGP
AMS252     ELSE
AMS252          MOVE        099               TO  MCTS-GRUPO
AMS252          MOVE        014               TO  MCTS-SUBGP.
AMS252
AMS252     IF   CCART OF DESOV004 EQUAL 'C'  AND
AMS252          WRK-TIPO-LOTE     EQUAL 'C'
AMS252          MOVE 3                        TO  MCTS-CONTA
AMS252          MOVE 5                        TO  MCTS-DIGITO
AMS252     ELSE
AMS252          MOVE 1                        TO  MCTS-CONTA
AMS252          MOVE 9                        TO  MCTS-DIGITO.
AMS252
AMS252     MOVE 845                           TO  MCTS-LANCAMENTO
AMS252     MOVE 2                             TO  MCTS-DEB-CRED
AMS252     MOVE WRK-VTARIF-TAC                TO  MCTS-VALOR
AMS252     PERFORM  40000-MONTA-FIXOS.
      *---------------------------------------------------------------*
       18100-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       18110-GRAVA-FORMA-ANTIGA        SECTION.
      *---------------------------------------------------------------*

      *----( CREDITO, RAZAO 65.31/32 PELO VALOR DOS JUROS )

           IF   CTPO-CTA-CORR OF  OBJEV001  EQUAL    '00'
                MOVE  065                     TO  MCTS-GRUPO
                MOVE  032                     TO  MCTS-SUBGP
           ELSE
                MOVE  065                     TO  MCTS-GRUPO
                MOVE  031                     TO  MCTS-SUBGP.

           MOVE 1                             TO  MCTS-CONTA
           MOVE 9                             TO  MCTS-DIGITO
           MOVE 7                             TO  MCTS-LANCAMENTO
           MOVE WRK-VJURO                     TO  MCTS-VALOR
           PERFORM  40000-MONTA-FIXOS.


      *---------------------------------------------------------------*
       18110-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       18120-GRAVA-PESSOA-CARTEIRA     SECTION.
      *---------------------------------------------------------------*

      *==> GRAVA RENDAS A APROPRIAR PESSOA JURIDICA

LOH        MOVE  007                                TO  MCTS-LANCAMENTO
           MOVE  065                                TO  MCTS-GRUPO
           MOVE  031                                TO  MCTS-SUBGP

           IF    ACU-RENDAS-J-CARTEIRA-A-T  GREATER ZEROS
                 MOVE  0000001                      TO  MCTS-CONTA
                 MOVE  2                            TO  MCTS-DEB-CRED
                 MOVE  ACU-RENDAS-J-CARTEIRA-A-T    TO  MCTS-VALOR
                 PERFORM  40000-MONTA-FIXOS.

           IF    ACU-RENDAS-J-CARTEIRA-B-E  GREATER ZEROS
                 MOVE  0000004                      TO  MCTS-CONTA
                 MOVE  3                            TO  MCTS-DIGITO
                 MOVE  2                            TO  MCTS-DEB-CRED
                 MOVE  ACU-RENDAS-J-CARTEIRA-B-E    TO  MCTS-VALOR
                 PERFORM  40000-MONTA-FIXOS.

           IF    ACU-RENDAS-J-CARTEIRA-D    GREATER ZEROS
                 MOVE  0000002                      TO  MCTS-CONTA
                 MOVE  7                            TO  MCTS-DIGITO
                 MOVE  2                            TO  MCTS-DEB-CRED
                 MOVE  ACU-RENDAS-J-CARTEIRA-D      TO  MCTS-VALOR
                 PERFORM  40000-MONTA-FIXOS.

           IF    ACU-RENDAS-J-CARTEIRA-C    GREATER ZEROS
                 MOVE  0000005                      TO  MCTS-CONTA
                 MOVE  2                            TO  MCTS-DEB-CRED
                 MOVE  ACU-RENDAS-J-CARTEIRA-C      TO  MCTS-VALOR
                 PERFORM  40000-MONTA-FIXOS.

      *==> GRAVA RENDAS A APROPRIAR PESSOA FISICA

           MOVE  065                                TO  MCTS-GRUPO
           MOVE  032                                TO  MCTS-SUBGP

           IF    ACU-RENDAS-F-CARTEIRA-B-E  GREATER ZEROS
                 MOVE  0000004                      TO  MCTS-CONTA
                 MOVE  3                            TO  MCTS-DIGITO
                 MOVE  2                            TO  MCTS-DEB-CRED
                 MOVE  ACU-RENDAS-F-CARTEIRA-B-E    TO  MCTS-VALOR
                 PERFORM  40000-MONTA-FIXOS.

           IF    ACU-RENDAS-F-CARTEIRA-D    GREATER ZEROS
                 MOVE  0000002                      TO  MCTS-CONTA
                 MOVE  7                            TO  MCTS-DIGITO
                 MOVE  2                            TO  MCTS-DEB-CRED
                 PERFORM  40000-MONTA-FIXOS.

           IF    ACU-RENDAS-F-CARTEIRA-C    GREATER ZEROS
                 MOVE  0000006                      TO  MCTS-CONTA
                 MOVE  'P'                          TO  MCTS-DIGITO
                 MOVE  2                            TO  MCTS-DEB-CRED
                 MOVE  ACU-RENDAS-F-CARTEIRA-C      TO  MCTS-VALOR
                 PERFORM  40000-MONTA-FIXOS.

      *---------------------------------------------------------------*
       18120-EXIT.                   EXIT.
      *---------------------------------------------------------------*
           EJECT
      *---------------------------------------------------------------*
       19000-FITA4S-AGENCIA            SECTION.
      *---------------------------------------------------------------*

           IF WRK-VRTIT-AGENCIA EQUAL ZEROS
              GO  TO  19000-EXIT.


STF        MOVE 4130                          TO  MCTS-AGENCIA
           MOVE 028                           TO  MCTS-GRUPO
           MOVE 001                           TO  MCTS-SUBGP
           MOVE 1                             TO  MCTS-CONTA
           MOVE 9                             TO  MCTS-DIGITO
           MOVE 7                             TO  MCTS-LANCAMENTO
           PERFORM 16000-MONTAR-DIA-SUB
           MOVE WRK-CHAVE-DIAGE-R             TO  MCTS-NUMERO-DOC
           MOVE 1                             TO  MCTS-DEB-CRED
           MOVE WRK-VRTIT-AGENCIA             TO  MCTS-VALOR
           PERFORM  40000-MONTA-FIXOS.

      *----( CREDITO, RAZAO 33.01 P/TOT.BRUTO TIT.COMPL.- AGENCIA )

STF        MOVE 4130                          TO  MCTS-AGENCIA
           MOVE 033                           TO  MCTS-GRUPO
           MOVE 001                           TO  MCTS-SUBGP
           MOVE 1                             TO  MCTS-CONTA
           MOVE 9                             TO  MCTS-DIGITO
           PERFORM 16000-MONTAR-DIA-SUB
           MOVE WRK-CHAVE-DIAGE-R             TO  MCTS-NUMERO-DOC
           MOVE 2                             TO  MCTS-DEB-CRED
           MOVE WRK-VRTIT-AGENCIA             TO  MCTS-VALOR
           PERFORM  40000-MONTA-FIXOS.

      *----( CREDITO, RAZAO 11.10 PELO VALOR DO IOF DA OPER. N/AGENCIA )

           MOVE WRK-AGENCIA-ANT               TO  MCTS-AGENCIA
      **** MOVE 11100                         TO  FIT4-RAZAO
           MOVE 011                           TO  MCTS-GRUPO
           MOVE 010                           TO  MCTS-SUBGP
           MOVE 1                             TO  MCTS-CONTA
           MOVE 9                             TO  MCTS-DIGITO
           MOVE 7                             TO  MCTS-LANCAMENTO
           MOVE 2                             TO  MCTS-DEB-CRED
           MOVE WRK-VRIOF-AGENCIA             TO  MCTS-VALOR

           MOVE WRK-DD-DIA-AGE                TO  WRK-CHAVE-DD-DG
           MOVE WRK-AGENCIA-ANT               TO  WRK-CHAVE-AG-DG
           PERFORM  40000-MONTA-FIXOS.

      *----( DEBITO, RAZAO 11.10 P/TOT.IOF - AGENCIA )

           MOVE 4101                          TO  MCTS-AGENCIA
           MOVE 011                           TO  MCTS-GRUPO
           MOVE 010                           TO  MCTS-SUBGP
           MOVE 1                             TO  MCTS-CONTA
           MOVE 9                             TO  MCTS-DIGITO
           MOVE 7                             TO  MCTS-LANCAMENTO
           PERFORM 17000-MONTAR-DIA-AGENCIA
           MOVE WRK-CHAVE-DIAGE-R             TO  MCTS-NUMERO-DOC
           MOVE 1                             TO  MCTS-DEB-CRED
           MOVE WRK-VRIOF-AGENCIA             TO  MCTS-VALOR
           PERFORM  40000-MONTA-FIXOS.

      *------------------------*
       19000-EXIT.         EXIT.
      *------------------------*

       20000-FITA4S-GERAL              SECTION.
      *---------------------------------------------------------------*

      *----( CREDITO, RAZAO 17.34 P/TOT.IOF DE TODAS AS AGENCIAS )

           MOVE 4101                          TO  MCTS-AGENCIA
           MOVE 017                           TO  MCTS-GRUPO
           MOVE 034                           TO  MCTS-SUBGP
           MOVE 301                           TO  MCTS-CONTA
           MOVE 8                             TO  MCTS-DIGITO
           MOVE 7                             TO  MCTS-LANCAMENTO
           MOVE DIA-DOPER1                    TO  DIA-NUMDOCTO
           MOVE MES-DOPER1                    TO  MES-NUMDOCTO
           MOVE ANO-DOPER1                    TO  ANO-NUMDOCTO
           MOVE WRK-DATA-NUM-DOCTO-R          TO  MCTS-NUMERO-DOC
           MOVE 2                             TO  MCTS-DEB-CRED
           MOVE WRK-VRIOF-GERAL               TO  MCTS-VALOR
           PERFORM  40000-MONTA-FIXOS.

      *------------------------*
      *------------------------*

      *----------------------------------------------*
       21000-FETCH-CURSOR-C4      SECTION.
      *----------------------------------------------*

           EXEC SQL
              FETCH     C4   INTO
                     :DESOV004.COPER-DESC-TITLO,
                     :DESOV004.DOPER-DESC-TITLO,
                     :DESOV004.PNEGOC-OPER-DESC,
                     :DESOV004.DINIC-OPER-DESC,
                     :DESOV004.DFNAL-OPER-DESC,
                     :DESOV004.QTITLO-OPER-DESC,
                     :DESOV004.VSOMA-TITLO-DESC,
                     :DESOV004.DANO-AUTRZ,
                     :DESOV004.CSEQ-NVEL-AUTRZ,
                     :DESOV004.CSIT-LIM-SUPE,
                     :DESOV004.CSIT-TX-ESPCL,
                     :DESOV004.CSIT-OPER-DESC,
                     :DESOV004.HATULZ,
                     :DESOV004.CJUNC-DEPDC,
                     :DESOV004.CCTA-CORR,
                     :DESOV004.CCART,
                     :DESOV004.CATVDD-OPER-DESC,
                     :DESOV004.CORIGE-REC,
                     :DESOV004.CID-PSSOA-DESC,
                     :DESOV004.CCTBIL-OPER-DESC,
                     :DESOV004.DANO-BASE
                          :NULL-DANO-BASE,
                     :DESOV004.CJUNC-DEPDC-STUDO
                          :NULL-CJUNC-DEPDC-STUDO,
                     :DESOV004.CSEQ-STUDO
                          :NULL-CSEQ-STUDO
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS AND +100)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01OPER_DESC_TITLO'     TO ERR-DBD-TAB
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0005              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

           IF  SQLCODE   EQUAL  +100
               MOVE    'FIM'   TO  WRK-FIM.

           IF  SQLCODE  EQUAL  ZEROS
               IF   NULL-DANO-BASE  EQUAL  -1
               OR   NULL-CJUNC-DEPDC-STUDO  EQUAL -1
               OR   NULL-CSEQ-STUDO EQUAL -1
                    MOVE  ZEROS  TO  DANO-BASE          OF  DESOV004
                                     CJUNC-DEPDC-STUDO  OF  DESOV004
                                     CSEQ-STUDO         OF  DESOV004.

      *------------------------*
       21000-EXIT.         EXIT.
      *------------------------*
           EJECT
       22000-FETCH-CURSOR-C5      SECTION.
      *----------------------------------------------*

           EXEC SQL
              FETCH     C5   INTO
                         :DESOV005.COPER-DESC-TITLO,
                         :DESOV005.CDIFDR-TITLO-DESC,
                         :DESOV005.DEMIS-TITLO-DESC,
                         :DESOV005.DVCTO-TITLO-DESC,
                         :DESOV005.VBRUTO-TITLO-DESC,
                         :DESOV005.CSEU-NRO,
                         :DESOV005.CSIT-TITLO-DESC,
                         :DESOV005.PDESC-INDCD-ECONM,
                         :DESOV005.IPSSOA,
                         :DESOV005.ELOGDR-NRO-COMPL,
                         :DESOV005.CCGC-CPF,
                         :DESOV005.CFLIAL-CGC,
                         :DESOV005.CCTRL-CPF-CGC,
                         :DESOV005.CTITLO-INFMD-SCTRO,
                         :DESOV005.CFUNC-BDSCO,
                         :DESOV005.CCEP,
                         :DESOV005.CCEP-COMPL,
                         :DESOV005.CJUNC-DEPDC,
                         :DESOV005.CCTA-CORR,
                         :DESOV005.CIDTFD-INDCD-TARIF,
                         :DESOV005.CIDTFD-INDCD-JURO,
                         :DESOV005.CIDTFD-INDCD-IOF,
                         :DESOV005.CIDTFD-INDCD-MORA,
                         :DESOV005.CCART,
                         :DESOV005.CPRZ-CART-DESC,
STF                      :DESOV005.CIDTFD-PRODT-CATAO
STF                           :WRK-CIDTFD-PRODT-CATAO-NULL
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS AND +100)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01TITLO_OPER_DESC'     TO ERR-DBD-TAB
                MOVE 'FETCH'           TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

           IF  SQLCODE = +100
               MOVE   'FIM'   TO WRK-FIM-DESO5.

      *------------------------*
       22000-EXIT.          EXIT.
      *------------------------*
           EJECT
      *----------------------------------------------*
       23000-FETCH-CURSOR-C1      SECTION.
      *----------------------------------------------*

           EXEC SQL
              FETCH     C1   INTO
                  :DESOV001.CCART,
                  :DESOV001.CTPO-PSSOA,
                  :DESOV001.CMODLD-ATULZ,
                  :DESOV001.CFUNC-BDSCO,
                  :DESOV001.CTPO-GARNT
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS AND +100)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01PARM-OPER-DESC '     TO ERR-DBD-TAB
                MOVE 'FETCH'           TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0008              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

      *------------------------*
       23000-EXIT.          EXIT.
      *------------------------*

      *---------------------------------------------------------------*
       24000-OPEN-CURSOR-C4   SECTION.
      *---------------------------------------------------------------*
              OPEN      C4
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01OPER_DESC_TITLO'     TO ERR-DBD-TAB
                MOVE 'OPEN'            TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0009              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

      *----------------------*
       24000-EXIT.        EXIT.
      *----------------------*
           EJECT
      *---------------------------------------------------------------*
       25000-CLOSE-CURSOR-C4      SECTION.
      *---------------------------------------------------------------*
              CLOSE     C4
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01OPER_DESC_TITLO'     TO ERR-DBD-TAB
                MOVE 'CLOSE'           TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0011              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

      *---------------------*
       25000-EXIT.     EXIT.
      *---------------------*
           EJECT
      *---------------------------------------------------------------*
       26000-OPEN-CURSOR-C5   SECTION.
      *---------------------------------------------------------------*
              OPEN      C5
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01TITLO_OPER_DESC'     TO ERR-DBD-TAB
                MOVE 'OPEN'            TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0013              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

      *----------------------*
       26000-EXIT.        EXIT.
      *----------------------*
           EJECT
      *---------------------------------------------------------------*
       27000-CLOSE-CURSOR-C5      SECTION.
      *---------------------------------------------------------------*
              CLOSE     C5
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01TITLO_OPER_DESC'     TO ERR-DBD-TAB
                MOVE 'CLOSE'           TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0014              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

      *---------------------*
       27000-EXIT.     EXIT.
      *---------------------*
           EJECT
      *---------------------------------------------------------------*
       28000-OPEN-CURSOR-C1   SECTION.
      *---------------------------------------------------------------*
           EXEC SQL
              OPEN      C1
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01PARM-OPER-DESC '     TO ERR-DBD-TAB
                MOVE 'OPEN'            TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0015              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

      *----------------------*
       28000-EXIT.        EXIT.
      *----------------------*
           EJECT
      *---------------------------------------------------------------*
       29000-CLOSE-CURSOR-C1      SECTION.

           EXEC SQL
              CLOSE     C1
           END-EXEC.

           IF (SQLCODE   NOT  =    ZEROS)   OR
              (SQLWARN0       =    'W')
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01PARM-OPER-DESC '     TO ERR-DBD-TAB
                MOVE 'CLOSE'           TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0016              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM   31000-ROTINA-ERRO.

      *---------------------*
       29000-EXIT.     EXIT.
      *---------------------*
           EJECT
      *--------------------------*
      *--------------------------*

           IF   WRK-TIPO-POOL   EQUAL  15
                DISPLAY '********** DESE1060 **************'
                DISPLAY '*   ERRO NO ACESSO A POOL0015    *'
                DISPLAY '*    PROCESSAMENTO CANCELADO     *'
                DISPLAY '**********************************'.

            GOBACK.

       30000-EXIT.    EXIT.
      *---------------------*
           EJECT
      *--------------------------*
       31000-ROTINA-ERRO  SECTION.
      *--------------------------*

           MOVE  'DESE1060'   TO      ERR-PGM.

JAPI-U     CALL  'BRAD7100'   USING   WRK-BATCH
                                      SQLCA.

       31000-EXIT. EXIT.
      *---------------------*
           EJECT
      *---------------------------------------------------------------*
       32000-UPDATE-DESOV004 SECTION.
      *---------------------------------------------------------------*

POLEN***   ADD  1  TO  WRK-DISPL.
POLEN***   IF  WRK-DISPL  LESS  50
POLEN***       DISPLAY 'DESOV004 ATUALIZADO = '
POLEN***                                  COPER-DESC-TITLO OF DESOV004.
MVS*********************************************************************
MVS*********************************************************************
MVS*********************************************************************
MVS**** A SUBSTITUICAO DO UPDATE DA TABELA  DESOV004  POR GRAVACAO DE **
MVS**** ARQUIVO SEQUENCIAL FAZ-SE  NECESSARIO  PARA  NAO  PRENDER  AS **
MVS**** PAGINAS DA TABELA DESOV004.  O UPDATE PASSARA A SER REALIZADO **
MVS**** NO PROGRAMA DESE1068 UTILIZANDO O PROCESSO DE COMMIT/RESTART. **
MVS*********************************************************************
MVS*********************************************************************
MVS********EXEC SQL
MVS********   UPDATE   DB2PRD.V01OPER_DESC_TITLO
MVS********   SET CCTBIL_OPER_DESC = 2
MVS********   WHERE
MVS********        COPER_DESC_TITLO  =  :DESOV004.COPER-DESC-TITLO
MVS********END-EXEC.
MVS********
MVS********IF ( SQLCODE   NOT  =    ZEROS)   OR
MVS********   ( SQLWARN0       =    'W'   )
MVS********     MOVE 'DB2'             TO     ERR-TIPO-ACESSO
MVS********     MOVE 'V01OPER_DESC_TITLO'     TO ERR-DBD-TAB
MVS********     MOVE 'UPDATE'          TO     ERR-FUN-COMANDO
MVS********     MOVE SQLCODE           TO     ERR-SQL-CODE
MVS********     MOVE 0017              TO     ERR-LOCAL
MVS********     MOVE SPACES            TO     ERR-SEGM
MVS********     PERFORM   31000-ROTINA-ERRO.
MVS
MVS        MOVE  COPER-DESC-TITLO OF DESOV004  TO  SAI-COPER-DESC-TITLO.
MVS
MVS        WRITE  REG-ATUDES04  FROM  WRK-ATUDES04.
MVS
japi=a     ADD  1  TO  WRK-GRAV-ATUDES04.

      *--------------------------*
       32000-EXIT. EXIT.
      *--------------------------*

      /---------------------------------------------------------------*
       35000-SELECT-DESOV010 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              SELECT
                   COPER_DESC_TITLO,
                   PJURO
              INTO
                   :DESOV010.COPER-DESC-TITLO,
                   :DESOV010.PJURO
              WHERE
                  (COPER_DESC_TITLO  = :DESOV005.COPER-DESC-TITLO)
           END-EXEC.

           IF ( SQLCODE   NOT  =    ZEROS  AND  +100 )   OR
              ( SQLWARN0       =    'W'   )
                MOVE 'DB2'             TO     ERR-TIPO-ACESSO
                MOVE 'V01OPER_TX_ESPCL'  TO   ERR-DBD-TAB
                MOVE 'SELECT'          TO     ERR-FUN-COMANDO
                MOVE SQLCODE           TO     ERR-SQL-CODE
                MOVE 0018              TO     ERR-LOCAL
                MOVE SPACES            TO     ERR-SEGM
                PERFORM                31000-ROTINA-ERRO.

           IF ( SQLCODE    EQUAL    +100)
                MOVE 'N'                 TO       MOVC-ESPNO-S
           ELSE
                MOVE 'E'                 TO       MOVC-ESPNO-S
                MOVE PJURO OF DESOV010   TO       MOVC-TXAPL-S.

       35000-EXIT. EXIT.
      *--------------------------*
           EJECT
      *--------------------------*
       40000-MONTA-FIXOS  SECTION.
      *--------------------------*

           IF     MCTS-VALOR  EQUAL  ZEROS
                  GO TO 40000-EXIT.

           MOVE   ZEROS                       TO  MCTS-DIA-VINCULO
           MOVE   'DESO'                      TO  MCTS-CTRO-CUSTO
           MOVE   4455                        TO  MCTS-SUBC-CRS
           MOVE   'LA'                        TO  MCTS-SERVICO
           MOVE   SPACES                      TO  MCTS-TIPO-ENTRADA
           MOVE   'N'                         TO  MCTS-SN
           MOVE   SPACES                      TO  MCTS-ORIGEM
           MOVE   ZEROS                       TO  MCTS-DIA-LA
           MOVE   ZEROS                       TO  MCTS-DATA-ORIGINAL
           MOVE   SPACES                      TO  MCTS-TRILHA

           WRITE    REG-FITA4S              FROM  REG-WORK-MCTS

           ADD      1                         TO  WRK-TOTFITA4S.

       40000-EXIT.    EXIT.
      *---------------------*
