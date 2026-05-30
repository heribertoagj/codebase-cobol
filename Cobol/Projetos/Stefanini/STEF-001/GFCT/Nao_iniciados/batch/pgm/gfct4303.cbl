      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4303.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4303                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: ANTONIO PAGNOCCA NETO                    *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........:                                          *
      *                                                                *
      *    DATA.............: MAR/2011                                 *
      *                                                                *
      *    PROJETO..........: CARTAO BONUS CELULAR                     *
      *                                                                *
      *    OBJETIVO.........: SELECIONA E COMPLEMENTA MOVTO, GESTAO DO *
      *                       DIA COM DADOS DA TARIFA CELULAR.         *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ENTMWATO                          GFCTWAOT              *
      *        ENTMWAL2                          GFCTWAL2              *
      *        SAIMWATO                          GFCTWAOT              *
      *        DESPREZA                          GFCTWAL2              *
      *        OCORRENC                          GFCTWAOT              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *      BRAD0560 - OBTER NOME DO JOB.                             *
      *      BRAD7100 - TRATAR ERRO                                    *
      *      BRAD7600 - BUSCAR DATA                                    *
      *                                                                *
PROC05*================================================================*
PROC05*                U L T I M A   A L T E R A C A O                 *
PROC05*================================================================*
PROC05*    SONDA-PROCWORK                                              *
PROC05*    DATA........:   MAIO/2011                                   *
PROC05*----------------------------------------------------------------*
PROC05*    OBJETIVO:  ALTERACAO NAS MENSAGENS PARA                     *
PROC05*               FACILITAR PARA GESTOR                            *
PROC05*================================================================*
022012*----------------------------------------------------------------*
022012*                 U L T I M A   A L T E R A C A O                *
022012*----------------------------------------------------------------*
022012*        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
022012*----------------------------------------------------------------*
022012*                                                                *
022012*    PROGRAMADOR.:  FABRICA         - SONDA/PROCWORK             *
022012*    ANALISTA....:  PAGNOCCA        - SONDA/PROCWORK             *
022012*    DATA........:  FEV / 2012                                   *
022012*    OBJETIVO....:  PROJETO CELULAR 09 DIGITOS.                  *
022012*                                                                *
022012*    OBSERVACAO..:  REALIZADO APENAS RECOMPILACAO PARA EFETIVAR  *
022012*                   ATUALIZACAO NA(S) BOOK(S)/DCLGEN(S).         *
022012*                                                                *
022012*================================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ENTMWATO ASSIGN TO UT-S-ENTMWATO
                      FILE STATUS IS WRK-FS-ENTMWATO.

           SELECT ENTMWAL2 ASSIGN TO UT-S-ENTMWAL2
                      FILE STATUS IS WRK-FS-ENTMWAL2.

           SELECT SAIMWATO ASSIGN TO UT-S-SAIMWATO
                      FILE STATUS IS WRK-FS-SAIMWATO.

           SELECT DESPREZA ASSIGN TO UT-S-DESPREZA
                      FILE STATUS IS WRK-FS-DESPREZA.

           SELECT OCORRENC ASSIGN TO UT-S-OCORRENC
                      FILE STATUS IS WRK-FS-OCORRENC.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 450               *
      *---------------------------------------------------------------*

       FD  ENTMWATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ENTMWATO             PIC X(450).

      *---------------------------------------------------------------*
      *   INPUT:      ORG. SEQUENCIAL   -   LRECL = 187               *
      *---------------------------------------------------------------*

       FD  ENTMWAL2
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ENTMWAL2             PIC X(187).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 450               *
      *---------------------------------------------------------------*

       FD  SAIMWATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SAIMWATO             PIC X(450).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 187               *
      *---------------------------------------------------------------*

       FD  DESPREZA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-DESPREZA             PIC X(187).

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 450               *
      *---------------------------------------------------------------*

       FD  OCORRENC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-OCORRENC             PIC X(450).

      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32)  VALUE  '*  INICIO DA WORKING GFCT4303  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TIMESTAMP.
               10  WRK-ANO             PIC  9(004)         VALUE ZEROS.
               10  WRK-MES             PIC  9(002)         VALUE ZEROS.
               10  WRK-DIA             PIC  9(002)         VALUE ZEROS.
               10  WRK-HHMMSS          PIC  X(012)         VALUE SPACES.

       01  WRK-HORA.
           05  WRK-HORA-HH             PIC  9(002)         VALUE ZEROS.
           05  WRK-HORA-MM             PIC  9(002)         VALUE ZEROS.
           05  WRK-HORA-SS             PIC  9(002)         VALUE ZEROS.
           05  WRK-HORA-MMMMMM         PIC  9(006)         VALUE ZEROS.

       01  WRK-TIMESTAMP-INTEIRO.
           05  WRK-ANO-TIMESTAMP       PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-MES-TIMESTAMP       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-DIA-TIMESTAMP       PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-HS-TIMESTAMP        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MM-TIMESTAMP        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-SS-TIMESTAMP        PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MMMMMM-TIMESTAMP    PIC  9(006)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS AUXILIARES           *'.
      *---------------------------------------------------------------*

       01  WRK-TAM-05-COM-S            PIC -9(005)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-05-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-05-SEM-S        PIC  9(005).

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*   AREAS DE FILE STATUS       *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ENTMWATO             PIC X(02) VALUE SPACES.
       77  WRK-FS-ENTMWAL2             PIC X(02) VALUE SPACES.
       77  WRK-FS-SAIMWATO             PIC X(02) VALUE SPACES.
       77  WRK-FS-DESPREZA             PIC X(02) VALUE SPACES.
       77  WRK-FS-OCORRENC             PIC X(02) VALUE SPACES.

       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32)  VALUE  '* ACUMULADORES                 *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ENTMWATO          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-LIDOS-ENTMWAL2          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-SAIMWATO          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-DESPREZA          PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-GRAVA-OCORRENC          PIC 9(09) COMP-3    VALUE ZEROS.

       77  ACU-NAO-ENCON-ENTMWATO      PIC 9(09) COMP-3    VALUE ZEROS.
       77  ACU-NAO-ENCON-ENTMWAL2      PIC 9(09) COMP-3    VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* MASCARAS DE EDICAO DE TOTAIS *'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-MASK-LIDOS-ENTMWATO PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-LIDOS-ENTMWAL2 PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-SAIMWATO PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-DESPREZA PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-GRAVA-OCORRENC PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.

           05  WRK-MASK-NAO-ENTMWATO   PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
           05  WRK-MASK-NAO-ENTMWAL2   PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.


      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* AREAS BRAD0560               *'.
      *---------------------------------------------------------------*

       01  WRK-P0560-JOBNAME           PIC X(08)           VALUE SPACES.
       01  WRK-P0560-JOBNUMBER         PIC 9(05)           VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '* AREAS CHAVES                 *'.
      *---------------------------------------------------------------*
       01  WRK-CHV-ENTMWATO.
           05 WRK-WAOT-CSERVC-TARIF    PIC 9(05)           VALUE ZEROS.
           05 WRK-WAOT-DOCOR-EVNTO     PIC X(10)           VALUE SPACES.

       01  WRK-CHV-ENTMWAL2.
           05 WRK-WAL2-CSERVC-TARIF    PIC 9(05)           VALUE ZEROS.
           05 WRK-WAL2-DOCOR-EVNTO     PIC X(10)           VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER      PIC X(32) VALUE '*  AREA SAIMWATO/OCORRENC   *'.
      *---------------------------------------------------------------*

       01  WRK-REG-GFCTWAOT.
           05  WRK-DADOS-EVENTO.
               10 WRK-CHAVE-EVENTO.
                  15 WRK-CROTNA-ORIGE-MOVTO PIC  X(04)    VALUE SPACES.
                  15 WRK-DENVIO-MOVTO-TARIF PIC  X(10)    VALUE SPACES.
                  15 WRK-CNRO-ARQ-MOVTO     PIC S9(02)V
                                                   COMP-3 VALUE ZEROS.
                  15 WRK-CSEQ-MOVTO         PIC S9(11)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CSERVC-TARIF          PIC S9(05)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CMIDIA-SERVC-MOVTO    PIC S9(05)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CPERIF-SERVC-MOVTO    PIC S9(05)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-QEVNTO-AGRUP          PIC S9(06)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-HPREST-SERVC-MOVTO    PIC  X(08)    VALUE SPACES.
               10 WRK-CTPO-CTA-MOVTO        PIC S9(01)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CBCO-DSTNO-MOVTO      PIC S9(03)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CAG-DSTNO-MOVTO       PIC S9(05)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CCTA-DSTNO-MOVTO      PIC S9(13)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CPAB-DSTNO-MOVTO      PIC S9(05)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CAG-CTLZA-DEB         PIC S9(05)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-CCTA-CTLZA-DEB        PIC S9(13)V
                                                   COMP-3 VALUE ZEROS.
               10 WRK-VTARIF-BRUTO-MOVTO    PIC S9(09)V9(2)
                                                   COMP-3 VALUE ZEROS.
               10 WRK-VTARIF-LIQ-MOVTO      PIC S9(09)V9(2)
                                                   COMP-3 VALUE ZEROS.
               10 WRK-VTARIF-DEB-MOVTO      PIC S9(09)V9(2)
                                                   COMP-3 VALUE ZEROS.
               10 WRK-DAGNDA-DEB-MOVTO      PIC  X(10)    VALUE SPACES.
               10 WRK-DEFETV-DEB-MOVTO      PIC  X(10)    VALUE SPACES.
               10 WRK-QMAX-TENTV-DEB        PIC S9(03)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-QTENTV-DEB-MOVTO      PIC S9(03)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CSIT-EVNTO-RECBD      PIC S9(02)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CIDTFD-PGMC-COBR      PIC S9(02)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-DOCOR-EVNTO           PIC  X(10)     VALUE SPACES.
               10 WRK-VTARIF-CADTR-SIST     PIC S9(09)V9(2)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CINDCD-TARIF-PCELD    PIC  X(1)      VALUE SPACES.
               10 WRK-QREG-INFMD-BONIF      PIC S9(5)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CINDCD-TARIF-EXCED    PIC S9(1)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CDOCTO-EXTRT          PIC S9(9)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-DANO-REFT-EXTRT       PIC S9(4)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CFUNC-SOLCT-EXTRT     PIC S9(9)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CTERM                 PIC  X(8)      VALUE SPACES.
               10 WRK-RJUSTF-SOLTC-EXTRT    PIC  X(30)     VALUE SPACES.
           05  WRK-DADOS-TARIFA-CEL.
               10 WRK-VREPASS-CRED-CEL      PIC S9(09)V9(2)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-QTDIAS-CRED-REJ       PIC S9(05)V
                                                   COMP-3  VALUE ZEROS.
           05  WRK-DADOS-ADESAO.
               10 WRK-CSERVC-TARIF-ADES     PIC S9(05)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-HINCL-REG             PIC  X(26)     VALUE SPACES.
               10 WRK-DINIC-ADSAO-INDVD     PIC  X(10)     VALUE SPACES.
               10 WRK-DFIM-ADSAO-INDVD      PIC  X(10)     VALUE SPACES.
           05  WRK-DADOS-CELULAR.
               10 WRK-NSEQ-REG-CLULR        PIC S9(05)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-COPER-FONE-MOVEL      PIC S9(09)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CDDD-FONE-MOVEL       PIC S9(03)V
                                                   COMP-3  VALUE ZEROS.
               10 WRK-CFONE-MOVEL           PIC S9(08)V
                                                   COMP-3  VALUE ZEROS.
           05  WRK-DADOS-CLIENTE.
               10 WRK-NOME-CLIE             PIC  X(40)     VALUE SPACES.
               10 WRK-CGC-CPF.
ST25X6*           15  WRK-CNPJ-NUM          PIC  9(09)
ST25X6*                                            COMP-3  VALUE ZEROS.
ST25X6*           15  WRK-CNPJ-FIL          PIC  9(05)
ST25X6*                                            COMP-3  VALUE ZEROS.
ST25X6            15  WRK-CNPJ-NUM          PIC  X(09)     VALUE SPACES.
ST25X6            15  WRK-CNPJ-FIL          PIC  X(04)     VALUE SPACES.  
                  15  WRK-CTRL              PIC  9(02)     VALUE ZEROS.
               10 WRK-NRO-CEP               PIC  9(05)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-COMPL-CEP             PIC  9(03)     VALUE ZEROS.
               10 WRK-POSTO-SERV            PIC  9(03)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-RAMO-ATIV             PIC  9(05)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-DT-ABERT              PIC  9(09)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-DT-ULTMV              PIC  9(09)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-DT-NASC               PIC  9(09)
                                                   COMP-3  VALUE ZEROS.
               10 WRK-SEXO                  PIC  X(01)     VALUE SPACES.
               10 WRK-TP-CHEQ               PIC  X(01)     VALUE SPACES.
               10 WRK-TP-EXTR               PIC  X(01)     VALUE SPACES.
           05  WRK-OC-ERRO.
               10 WRK-JOBNAME               PIC  X(08)     VALUE SPACES.
               10 WRK-PGMNAME               PIC  X(08)     VALUE SPACES.
               10 WRK-RECURSO               PIC  X(08)     VALUE SPACES.
               10 WRK-DATAHORA              PIC  X(26)     VALUE SPACES.
               10 WRK-COD-RETORNO           PIC  9(03)     VALUE ZEROS.
               10 WRK-DESC-OC               PIC  X(40)     VALUE SPACES.
           05  FILLER                       PIC  X(18)     VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER      PIC X(32) VALUE '* ENTMWATO/SAIMWATO/OCORRENC*'.
      *---------------------------------------------------------------*

       COPY 'GFCTWAOT'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*  AREAS ENTMWAL2/DESPREZA  *'.
      *---------------------------------------------------------------*

       COPY 'GFCTWAL2'.

      *---------------------------------------------------------------*
       01  FILLER    PIC X(32)   VALUE '*   AREAS TRATAMENTO ERRO    *'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32)  VALUE  '*  FIM  DA WORKING   GFCT4303  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*

      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           OPEN    INPUT   ENTMWATO
                           ENTMWAL2
                   OUTPUT  SAIMWATO
                           DESPREZA
                           OCORRENC

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-ROTINA-INICIAL.

           IF  ACU-LIDOS-ENTMWATO      EQUAL ZEROS  OR
               ACU-LIDOS-ENTMWAL2      EQUAL ZEROS
               GO                      TO  0000-FINALIZAR
           END-IF.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ENTMWATO     EQUAL '10'  AND
                   WRK-FS-ENTMWAL2     EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.


      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ENTMWATO
                   ENTMWAL2
                   SAIMWATO
                   DESPREZA
                   OCORRENC.

           MOVE    WRK-FECHAMENTO      TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           STOP    RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FILE-STATUS
      *===============================================================*
       1000-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-ENTMWATO.

           PERFORM 1200-TESTAR-FS-ENTMWAL2.

           PERFORM 1300-TESTAR-FS-SAIMWATO.

           PERFORM 1400-TESTAR-FS-DESPREZA.

           PERFORM 1500-TESTAR-FS-OCORRENC.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ENTMWATO
      *===============================================================*
       1100-TESTAR-FS-ENTMWATO SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ENTMWATO         NOT EQUAL '00'
               DISPLAY '************** GFCT4303 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ENTMWATO             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTMWATO
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4303 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ENTMWATO'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ENTMWAL2
      *===============================================================*
       1200-TESTAR-FS-ENTMWAL2 SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ENTMWAL2         NOT EQUAL '00'
               DISPLAY '************** GFCT4303 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*              ENTMWAL2             *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTMWAL2
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4303 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'ENTMWAL2'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-SAIMWATO
      *===============================================================*
       1300-TESTAR-FS-SAIMWATO SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SAIMWATO         NOT EQUAL '00'
               DISPLAY '************** GFCT4303 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               SAIMWATO            *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-SAIMWATO
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4303 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'SAIMWATO'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-DESPREZA
      *===============================================================*
       1400-TESTAR-FS-DESPREZA SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-DESPREZA         NOT EQUAL '00'
               DISPLAY '************** GFCT4303 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               DESPREZA            *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-DESPREZA
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4303 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'DESPREZA'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1400-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-OCORRENC
      *===============================================================*
       1500-TESTAR-FS-OCORRENC SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-OCORRENC         NOT EQUAL '00'
               DISPLAY '************** GFCT4303 *************'
               DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
               DISPLAY '*               OCORRENC            *'
               DISPLAY '*         FILE STATUS =  ' WRK-FS-OCORRENC
                                                  '         *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '************** GFCT4303 *************'
               MOVE WRK-OPERACAO       TO  WRK-OPERACAO-TXT
               MOVE 'OCORRENC'         TO  WRK-ARQ-TXT
               MOVE WRK-TEXTO          TO  ERR-TEXTO
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1500-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ROTINA-INICIAL
      *===============================================================*
       2000-ROTINA-INICIAL SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-ENTMWATO.

           IF  ACU-LIDOS-ENTMWATO       EQUAL   ZEROS
               DISPLAY '************* GFCT4303 *************'
               DISPLAY '*                                  *'
               DISPLAY '*    ARQUIVO ENTMWATO ESTA VAZIO   *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4303 *************'
           END-IF.

           PERFORM 2200-LER-ENTMWAL2.

           IF  ACU-LIDOS-ENTMWAL2       EQUAL   ZEROS
               DISPLAY '************* GFCT4303 *************'
               DISPLAY '*                                  *'
               DISPLAY '*    ARQUIVO ENTMWAL2 ESTA VAZIO   *'
               DISPLAY '*                                  *'
               DISPLAY '*        PROGRAMA ENCERRADO        *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT4303 *************'
           END-IF.

           CALL 'BRAD0560'             USING WRK-P0560-JOBNAME
                                             WRK-P0560-JOBNUMBER.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE SPACES             TO  WRK-P0560-JOBNAME
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ENTMWATO
      *===============================================================*
       2100-LER-ENTMWATO  SECTION.
      *---------------------------------------------------------------*

           READ ENTMWATO  INTO REG-GFCTWAOT.

           IF  WRK-FS-ENTMWATO         EQUAL  '10'
               MOVE HIGH-VALUES        TO  WRK-CHV-ENTMWATO
               INITIALIZE                  REG-GFCTWAOT
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ENTMWATO.

           MOVE WAOT-CSERVC-TARIF      TO  WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S       TO  WRK-WAOT-CSERVC-TARIF.
           MOVE WAOT-DOCOR-EVNTO       TO  WRK-WAOT-DOCOR-EVNTO.

           ADD  1                      TO  ACU-LIDOS-ENTMWATO.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ENTMWAL2
      *===============================================================*
       2200-LER-ENTMWAL2  SECTION.
      *---------------------------------------------------------------*

           READ ENTMWAL2  INTO REG-GFCTWAL2.

           IF  WRK-FS-ENTMWAL2         EQUAL  '10'
               MOVE HIGH-VALUES        TO  WRK-CHV-ENTMWAL2
               INITIALIZE                  REG-GFCTWAL2
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FS-ENTMWAL2.

           MOVE WAL2-CSERVC-TARIF      TO  WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S       TO  WRK-WAL2-CSERVC-TARIF.
           MOVE WAL2-DOCOR-EVNTO       TO  WRK-WAL2-DOCOR-EVNTO.

           ADD  1                      TO  ACU-LIDOS-ENTMWAL2.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           IF  WRK-CHV-ENTMWATO       EQUAL   WRK-CHV-ENTMWAL2
               PERFORM 3100-CHAVE-IGUAL
               PERFORM 2100-LER-ENTMWATO
           ELSE
             IF WRK-CHV-ENTMWATO     GREATER   WRK-CHV-ENTMWAL2
                PERFORM 3120-OCORR-TIPO01
                PERFORM 2200-LER-ENTMWAL2
             ELSE
                PERFORM 3110-OCORR-TIPO02
                PERFORM 2100-LER-ENTMWATO
             END-IF
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA CHAVE-IGUAL
      *===============================================================*
       3100-CHAVE-IGUAL SECTION.
      *---------------------------------------------------------------*

           PERFORM 3130-MOVIMENT-CAMPOS
                   UNTIL   WRK-CHV-ENTMWAL2 NOT EQUAL WRK-CHV-ENTMWATO.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ABRIR CURSOR
      *===============================================================*
       3110-OCORR-TIPO02 SECTION.
      *---------------------------------------------------------------*

           MOVE REG-GFCTWAOT           TO  WRK-REG-GFCTWAOT.

           MOVE WRK-P0560-JOBNAME      TO  WRK-JOBNAME
           MOVE 'GFCT4303'             TO  WRK-PGMNAME

           MOVE SPACES                 TO  WRK-RECURSO

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-HHMMSS             TO  WRK-HORA.
           MOVE WRK-HORA-HH            TO  WRK-HS-TIMESTAMP
           MOVE WRK-HORA-MM            TO  WRK-MM-TIMESTAMP
           MOVE WRK-HORA-SS            TO  WRK-SS-TIMESTAMP
           MOVE WRK-HORA-MMMMMM        TO  WRK-MMMMMM-TIMESTAMP

           MOVE WRK-ANO                TO  WRK-ANO-TIMESTAMP
           MOVE WRK-MES                TO  WRK-MES-TIMESTAMP
           MOVE WRK-DIA                TO  WRK-DIA-TIMESTAMP

           MOVE WRK-TIMESTAMP-INTEIRO  TO  WRK-DATAHORA

           MOVE ZEROS                  TO  WRK-COD-RETORNO

PROC05     MOVE 'ERRO-CADASTRO DESBATIDO COM EVENTO'
PROC05                                 TO  WRK-DESC-OC.
PROC05
           PERFORM 3510-GRAVAR-TIPO02.

      *---------------------------------------------------------------*
       3110-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER CURSOR
      *===============================================================*
       3120-OCORR-TIPO01 SECTION.
      *---------------------------------------------------------------*

           INITIALIZE  WRK-REG-GFCTWAOT.

           MOVE WAL2-CROTNA-ORIGE-MOVTO  TO  WRK-CROTNA-ORIGE-MOVTO
           MOVE WAL2-DENVIO-MOVTO-TARIF  TO  WRK-DENVIO-MOVTO-TARIF
           MOVE WAL2-CNRO-ARQ-MOVTO      TO  WRK-CNRO-ARQ-MOVTO
           MOVE WAL2-CSEQ-MOVTO          TO  WRK-CSEQ-MOVTO
           MOVE WAL2-CSERVC-TARIF        TO  WRK-CSERVC-TARIF
           MOVE WAL2-CMIDIA-SERVC-MOVTO  TO  WRK-CMIDIA-SERVC-MOVTO
           MOVE WAL2-CPERIF-SERVC-MOVTO  TO  WRK-CPERIF-SERVC-MOVTO
           MOVE WAL2-QEVNTO-AGRUP        TO  WRK-QEVNTO-AGRUP
           MOVE WAL2-HPREST-SERVC-MOVTO  TO  WRK-HPREST-SERVC-MOVTO
           MOVE WAL2-CTPO-CTA-MOVTO      TO  WRK-CTPO-CTA-MOVTO
           MOVE WAL2-CBCO-DSTNO-MOVTO    TO  WRK-CBCO-DSTNO-MOVTO
           MOVE WAL2-CAG-DSTNO-MOVTO     TO  WRK-CAG-DSTNO-MOVTO
           MOVE WAL2-CCTA-DSTNO-MOVTO    TO  WRK-CCTA-DSTNO-MOVTO
           MOVE WAL2-CPAB-DSTNO-MOVTO    TO  WRK-CPAB-DSTNO-MOVTO
           MOVE WAL2-CAG-CTLZA-DEB       TO  WRK-CAG-CTLZA-DEB
           MOVE WAL2-CCTA-CTLZA-DEB      TO  WRK-CCTA-CTLZA-DEB
           MOVE WAL2-VTARIF-BRUTO-MOVTO  TO  WRK-VTARIF-BRUTO-MOVTO
           MOVE WAL2-VTARIF-LIQ-MOVTO    TO  WRK-VTARIF-LIQ-MOVTO
           MOVE WAL2-VTARIF-DEB-MOVTO    TO  WRK-VTARIF-DEB-MOVTO
           MOVE WAL2-DAGNDA-DEB-MOVTO    TO  WRK-DAGNDA-DEB-MOVTO
           MOVE WAL2-DEFETV-DEB-MOVTO    TO  WRK-DEFETV-DEB-MOVTO
           MOVE WAL2-QMAX-TENTV-DEB      TO  WRK-QMAX-TENTV-DEB
           MOVE WAL2-QTENTV-DEB-MOVTO    TO  WRK-QTENTV-DEB-MOVTO
           MOVE WAL2-CSIT-EVNTO-RECBD    TO  WRK-CSIT-EVNTO-RECBD
           MOVE WAL2-CIDTFD-PGMC-COBR    TO  WRK-CIDTFD-PGMC-COBR
           MOVE WAL2-DOCOR-EVNTO         TO  WRK-DOCOR-EVNTO
           MOVE WAL2-VTARIF-CADTR-SIST   TO  WRK-VTARIF-CADTR-SIST
           MOVE WAL2-CINDCD-TARIF-PCELD  TO  WRK-CINDCD-TARIF-PCELD
           MOVE WAL2-QREG-INFMD-BONIF    TO  WRK-QREG-INFMD-BONIF
           MOVE WAL2-CINDCD-TARIF-EXCED  TO  WRK-CINDCD-TARIF-EXCED
           MOVE WAL2-CDOCTO-EXTRT        TO  WRK-CDOCTO-EXTRT
           MOVE WAL2-DANO-REFT-EXTRT     TO  WRK-DANO-REFT-EXTRT
           MOVE WAL2-CFUNC-SOLCT-EXTRT   TO  WRK-CFUNC-SOLCT-EXTRT
           MOVE WAL2-CTERM               TO  WRK-CTERM
           MOVE WAL2-RJUSTF-SOLTC-EXTRT  TO  WRK-RJUSTF-SOLTC-EXTRT

           MOVE WRK-P0560-JOBNAME        TO  WRK-JOBNAME
           MOVE 'GFCT4303'               TO  WRK-PGMNAME

           MOVE SPACES                   TO  WRK-RECURSO

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-HHMMSS             TO  WRK-HORA.
           MOVE WRK-HORA-HH            TO  WRK-HS-TIMESTAMP
           MOVE WRK-HORA-MM            TO  WRK-MM-TIMESTAMP
           MOVE WRK-HORA-SS            TO  WRK-SS-TIMESTAMP
           MOVE WRK-HORA-MMMMMM        TO  WRK-MMMMMM-TIMESTAMP

           MOVE WRK-ANO                TO  WRK-ANO-TIMESTAMP
           MOVE WRK-MES                TO  WRK-MES-TIMESTAMP
           MOVE WRK-DIA                TO  WRK-DIA-TIMESTAMP

           MOVE WRK-TIMESTAMP-INTEIRO  TO  WRK-DATAHORA

           MOVE ZEROS                    TO  WRK-COD-RETORNO

PROC05
PROC05     MOVE 'TARIFA/PACT NAO PERM. CREDITO CEL.'
PROC05                                   TO  WRK-DESC-OC.

           PERFORM 3520-GRAVAR-TIPO01.

           PERFORM 3530-GRAVAR-DESPREZA.

      *---------------------------------------------------------------*
       3120-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA MOVIMENTAR OS CAMPOS QUANDO CHAVE IGUAL
      *===============================================================*
       3130-MOVIMENT-CAMPOS SECTION.
      *---------------------------------------------------------------*

           MOVE REG-GFCTWAOT             TO  WRK-REG-GFCTWAOT.

           MOVE WAL2-CROTNA-ORIGE-MOVTO  TO  WRK-CROTNA-ORIGE-MOVTO
           MOVE WAL2-DENVIO-MOVTO-TARIF  TO  WRK-DENVIO-MOVTO-TARIF
           MOVE WAL2-CNRO-ARQ-MOVTO      TO  WRK-CNRO-ARQ-MOVTO
           MOVE WAL2-CSEQ-MOVTO          TO  WRK-CSEQ-MOVTO
           MOVE WAL2-CSERVC-TARIF        TO  WRK-CSERVC-TARIF
           MOVE WAL2-CMIDIA-SERVC-MOVTO  TO  WRK-CMIDIA-SERVC-MOVTO
           MOVE WAL2-CPERIF-SERVC-MOVTO  TO  WRK-CPERIF-SERVC-MOVTO
           MOVE WAL2-QEVNTO-AGRUP        TO  WRK-QEVNTO-AGRUP
           MOVE WAL2-HPREST-SERVC-MOVTO  TO  WRK-HPREST-SERVC-MOVTO
           MOVE WAL2-CTPO-CTA-MOVTO      TO  WRK-CTPO-CTA-MOVTO
           MOVE WAL2-CBCO-DSTNO-MOVTO    TO  WRK-CBCO-DSTNO-MOVTO
           MOVE WAL2-CAG-DSTNO-MOVTO     TO  WRK-CAG-DSTNO-MOVTO
           MOVE WAL2-CCTA-DSTNO-MOVTO    TO  WRK-CCTA-DSTNO-MOVTO
           MOVE WAL2-CPAB-DSTNO-MOVTO    TO  WRK-CPAB-DSTNO-MOVTO
           MOVE WAL2-CAG-CTLZA-DEB       TO  WRK-CAG-CTLZA-DEB
           MOVE WAL2-CCTA-CTLZA-DEB      TO  WRK-CCTA-CTLZA-DEB
           MOVE WAL2-VTARIF-BRUTO-MOVTO  TO  WRK-VTARIF-BRUTO-MOVTO
           MOVE WAL2-VTARIF-LIQ-MOVTO    TO  WRK-VTARIF-LIQ-MOVTO
           MOVE WAL2-VTARIF-DEB-MOVTO    TO  WRK-VTARIF-DEB-MOVTO
           MOVE WAL2-DAGNDA-DEB-MOVTO    TO  WRK-DAGNDA-DEB-MOVTO
           MOVE WAL2-DEFETV-DEB-MOVTO    TO  WRK-DEFETV-DEB-MOVTO
           MOVE WAL2-QMAX-TENTV-DEB      TO  WRK-QMAX-TENTV-DEB
           MOVE WAL2-QTENTV-DEB-MOVTO    TO  WRK-QTENTV-DEB-MOVTO
           MOVE WAL2-CSIT-EVNTO-RECBD    TO  WRK-CSIT-EVNTO-RECBD
           MOVE WAL2-CIDTFD-PGMC-COBR    TO  WRK-CIDTFD-PGMC-COBR
           MOVE WAL2-DOCOR-EVNTO         TO  WRK-DOCOR-EVNTO
           MOVE WAL2-VTARIF-CADTR-SIST   TO  WRK-VTARIF-CADTR-SIST
           MOVE WAL2-CINDCD-TARIF-PCELD  TO  WRK-CINDCD-TARIF-PCELD
           MOVE WAL2-QREG-INFMD-BONIF    TO  WRK-QREG-INFMD-BONIF
           MOVE WAL2-CINDCD-TARIF-EXCED  TO  WRK-CINDCD-TARIF-EXCED
           MOVE WAL2-CDOCTO-EXTRT        TO  WRK-CDOCTO-EXTRT
           MOVE WAL2-DANO-REFT-EXTRT     TO  WRK-DANO-REFT-EXTRT
           MOVE WAL2-CFUNC-SOLCT-EXTRT   TO  WRK-CFUNC-SOLCT-EXTRT
           MOVE WAL2-CTERM               TO  WRK-CTERM
           MOVE WAL2-RJUSTF-SOLTC-EXTRT  TO  WRK-RJUSTF-SOLTC-EXTRT.

           PERFORM 3500-GRAVAR-SAIMWATO.

           PERFORM 2200-LER-ENTMWAL2.

      *---------------------------------------------------------------*
       3130-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-SAIMWATO
      *===============================================================*
       3500-GRAVAR-SAIMWATO SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-SAIMWATO     FROM  WRK-REG-GFCTWAOT
           PERFORM 1300-TESTAR-FS-SAIMWATO.

           ADD 1                       TO  ACU-GRAVA-SAIMWATO.

      *---------------------------------------------------------------*
       3500-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-OCORRENCIA TIPO 02
      *===============================================================*
       3510-GRAVAR-TIPO02 SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-OCORRENC     FROM  WRK-REG-GFCTWAOT
           PERFORM 1500-TESTAR-FS-OCORRENC.

           ADD 1                       TO  ACU-GRAVA-OCORRENC
                                           ACU-NAO-ENCON-ENTMWATO.

      *---------------------------------------------------------------*
       3510-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-OCORRENCIA TIPO 01
      *===============================================================*
       3520-GRAVAR-TIPO01 SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-OCORRENC     FROM  WRK-REG-GFCTWAOT
           PERFORM 1500-TESTAR-FS-OCORRENC.

           ADD 1                       TO  ACU-GRAVA-OCORRENC
                                           ACU-NAO-ENCON-ENTMWAL2.

      *---------------------------------------------------------------*
       3520-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA GRAVAR-DESPREZA
      *===============================================================*
       3530-GRAVAR-DESPREZA SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           WRITE FD-REG-DESPREZA     FROM  REG-GFCTWAL2
           PERFORM 1400-TESTAR-FS-DESPREZA.

           ADD 1                       TO  ACU-GRAVA-DESPREZA.

      *---------------------------------------------------------------*
       3530-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

      *--> MOSTRAR TOTALIZACAO

           MOVE ACU-LIDOS-ENTMWATO     TO  WRK-MASK-LIDOS-ENTMWATO
           MOVE ACU-LIDOS-ENTMWAL2     TO  WRK-MASK-LIDOS-ENTMWAL2
           MOVE ACU-GRAVA-SAIMWATO     TO  WRK-MASK-GRAVA-SAIMWATO
           MOVE ACU-GRAVA-DESPREZA     TO  WRK-MASK-GRAVA-DESPREZA
           MOVE ACU-GRAVA-OCORRENC     TO  WRK-MASK-GRAVA-OCORRENC
           MOVE ACU-NAO-ENCON-ENTMWATO TO  WRK-MASK-NAO-ENTMWATO
           MOVE ACU-NAO-ENCON-ENTMWAL2 TO  WRK-MASK-NAO-ENTMWAL2

           DISPLAY '*********************** GFCT4303 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS   ENTMWATO.... : '
                                       WRK-MASK-LIDOS-ENTMWATO   ' *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS   ENTMWAL2.... : '
                                       WRK-MASK-LIDOS-ENTMWAL2   ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS SAIMWATO... : '
                                       WRK-MASK-GRAVA-SAIMWATO   ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS DESPREZA... : '
                                       WRK-MASK-GRAVA-DESPREZA   ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS OCORRENC... : '
                                       WRK-MASK-GRAVA-OCORRENC   ' *'.
           DISPLAY '* NAO LOCALIZADOS ENTMWATO (ERRO)....... : '
                                       WRK-MASK-NAO-ENTMWATO     ' *'.
           DISPLAY '* NAO LOCALIZADOS ENTMWAL2.............. : '
                                       WRK-MASK-NAO-ENTMWAL2     ' *'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT4303 *******************
      -                                                        '****'.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'GFCT4303'             TO  ERR-PGM

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
