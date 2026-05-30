      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2109.
       AUTHOR.     RICARDO PEREIRA DA SILVA.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT2109                                    *
      *    PROGRAMADOR.:   RICARDO PEREIRA DA SILVA    - SONDPROC/GP.50*
      *    ANALISTA....:   RODRIGO RIOS DO PRADO       - SONDPROC/GP.50*
      *    DATA........:   17/03/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CANCELAMENTO DE ADESOES EM LOTE.            *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                     CANADEEN                   I#GFCTC8        *
      *                     CANADESA                   I#GFCTC9        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                                         *
      *                   DB2PRD.PARM_DATA_PROCM     GFCTB0A1          *
      *                   DB2PRD.ADSAO_INDVD_PCOTE   GFCTB009          *
151209*                   DB2PRD.TADSAO_INDVD_CLULR  GFCTB0M2          *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    BRAD7C   - AREA CHAMADA BRAD7100                            *
      *----------------------------------------------------------------*
      *    MODULOS / BRADS:                                            *
      *    CKRS0100 - TRATAMENTO CHECKPOINT RESTART DB2                *
      *    CKRS1000 - CONEXAO COM DB2                                  *
      *    BRAD7100 - MODULO TRATAMENTO DE ERRO                        *
      *    BRAD0025 - SOMA DIAS NA DATA.                               *
      *================================================================*
080512*================================================================*
080512*     S O N D A   P R O C W O R K  -  C O N S U L T O R I A      *
080512*----------------------------------------------------------------*
080512*    PROGRAMA....: GFCT2109 - ALTERACAO                          *
080512*    PROGRAMADOR.: VINICIUS C. MADUREIRA - SONDA PROCWORK/GP.50  *
080512*    ANALISTA....: VINICIUS C. MADUREIRA - SONDA PROCWORK/GP.50  *
080512*    DATA........: 12/05/2008                                    *
080512*----------------------------------------------------------------*
080512*    OBJETIVO....: AJUSTE NO PROCESSO DE CANCELAMENTO.           *
080512*----------------------------------------------------------------*
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
022012*================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*

       FILE-CONTROL.

           SELECT CANADEEN   ASSIGN      TO UT-S-CANADEEN
           FILE STATUS                 IS WRK-FS-CANADEEN.

           SELECT CANADESA   ASSIGN      TO UT-S-CANADESA
           FILE STATUS                 IS WRK-FS-CANADESA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO                                             *
      *            ORG. SEQUENCIAL     - LRECL = 060                   *
      *----------------------------------------------------------------*

       FD  CANADEEN
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTC8'.

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO                                             *
      *            ORG. SEQUENCIAL     - LRECL = 150                   *
      *----------------------------------------------------------------*

       FD  CANADESA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTC9'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*   INICIO DA WORKING GFCT2109 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*         CONTADORES           *'.
      *----------------------------------------------------------------*

       01  WRK-ACU-LIDOS-CANADEEN      PIC  9(007) COMP-3  VALUE ZEROS.
080512 01  WRK-ACU-INSERIDOS           PIC  9(007) COMP-3  VALUE ZEROS.
       01  WRK-ACU-CANCELADOS          PIC  9(007) COMP-3  VALUE ZEROS.
       01  WRK-ACU-ADES-NAO-ENCONTR    PIC  9(007) COMP-3  VALUE ZEROS.
       01  WRK-ACU-ERROS               PIC  9(007) COMP-3  VALUE ZEROS.
       01  WRK-ACU-ADES-ENCONTR-PAS    PIC  9(007) COMP-3  VALUE ZEROS.
151209 01  WRK-ACU-ISRT-GFCTB0M2       PIC  9(007) COMP-3  VALUE ZEROS.
151209 01  WRK-ACU-ATLZ-GFCTB0M2       PIC  9(007) COMP-3  VALUE ZEROS.
151209 01  WRK-ACU-SEM-GFCTB0M2        PIC  9(007) COMP-3  VALUE ZEROS.
      *77  ACU-CHECKPOINT-RESTART      PIC  9(009) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC  X(008)         VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC  Z.ZZZ.ZZ9      VALUE ZEROS.
           05  WRK-ERRO-PROCESSO       PIC  X(001)         VALUE 'N'.

       01  WRK-CANADEEN.
           05  WRK-AGENCIA-ENTR        PIC  9(005)         VALUE ZEROS.
           05  WRK-CONTA-ENTR          PIC  9(007)         VALUE ZEROS.
           05  WRK-TARIFA-GFCT-ENTR    PIC  9(005)         VALUE ZEROS.
           05  WRK-TARIFA-LEGADO-ENTR  PIC  9(005)         VALUE ZEROS.
           05  WRK-CENTRO-CUSTO-ENTR   PIC  X(004)         VALUE SPACES.
           05  WRK-VALOR-ENTR          PIC  9(005)V99      VALUE ZEROS.

       01  WRK-CANADESA.
           05  WRK-AGENCIA-SAID        PIC  9(005)         VALUE ZEROS.
           05  WRK-CONTA-SAID          PIC  9(007)         VALUE ZEROS.
           05  WRK-TARIFA-GFCT-SAID    PIC  9(005)         VALUE ZEROS.
           05  WRK-TARIFA-LEGADO-SAID  PIC  9(005)         VALUE ZEROS.
           05  WRK-CENTRO-CUSTO-SAID   PIC  X(004)         VALUE SPACES.
           05  WRK-DATA-INIC-VIG-SAID  PIC  9(008)         VALUE ZEROS.
           05  WRK-DATA-FIM-VIG-SAID   PIC  9(008)         VALUE ZEROS.
           05  WRK-VALOR-SAID          PIC  9(005)V99      VALUE ZEROS.
           05  WRK-MENSAGEM-SAID       PIC  X(070)         VALUE SPACES.
           05  WRK-IND-ERRO-SAID       PIC  9(001)         VALUE ZEROS.

       01  WRK-DINIC-ADSAO-INDVD       PIC  X(010)         VALUE SPACES.
       01  WRK-DFIM-ADSAO-INDVD        PIC  X(010)         VALUE SPACES.
       01  WRK-CJUNC-DEPDC             PIC  9(005)         VALUE ZEROS.
       01  WRK-CCTA-CLI                PIC  9(007)         VALUE ZEROS.
       01  WRK-CSERVC-TARIF            PIC  9(005)         VALUE ZEROS.
       01  WRK-HINCL-REG               PIC  X(026)         VALUE SPACES.
151209 01  WRK-TIMESTAMP               PIC  X(026)         VALUE SPACES.
151209 01  WRK-NSEQ-REG-CLULR          PIC S9(005)  COMP-3 VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* VARIAVEIS DATA               *'.
      *----------------------------------------------------------------*

       01  WRK-DPROCM-ATU            PIC  X(010)         VALUE SPACES.
       01  WRK-DPROCM-ATU-R          REDEFINES WRK-DPROCM-ATU.
           05  WRK-DIA-ATU           PIC  9(002).
           05  WRK-PTO1              PIC  X(001).
           05  WRK-MES-ATU           PIC  9(002).
           05  WRK-PTO2              PIC  X(001).
           05  WRK-ANO-ATU           PIC  9(004).

       01  WRK-DPROCM-ATU-INV          PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROCM-ATU-INV-R        REDEFINES WRK-DPROCM-ATU-INV.
           05  WRK-ANO-ATU-INV         PIC  9(004).
           05  WRK-MES-ATU-INV         PIC  9(002).
           05  WRK-DIA-ATU-INV         PIC  9(002).

       01  WRK-DPROCM-PROX            PIC  X(010)         VALUE SPACES.
       01  WRK-DPROCM-PROX-R          REDEFINES WRK-DPROCM-PROX.
           05  WRK-DIA-PROX           PIC  9(002).
           05  WRK-PTO5               PIC  X(001).
           05  WRK-MES-PROX           PIC  9(002).
           05  WRK-PTO6               PIC  X(001).
           05  WRK-ANO-PROX           PIC  9(004).

       01  WRK-DPROCM-ATUU           PIC  X(010)         VALUE SPACES.
       01  WRK-DPROCM-ATUU-R         REDEFINES WRK-DPROCM-ATUU.
           05  WRK-DIA-ATUU          PIC  9(002).
           05  WRK-PTO3              PIC  X(001).
           05  WRK-MES-ATUU          PIC  9(002).
           05  WRK-PTO4              PIC  X(001).
           05  WRK-ANO-ATUU          PIC  9(004).

       01  WRK-DATA-0025E              PIC  9(009)        VALUE ZEROS.
       01  WRK-DATA-0025E-R            REDEFINES WRK-DATA-0025E.
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-0025E           PIC  9(004).
           05  WRK-MES-0025E           PIC  9(002).
           05  WRK-DIA-0025E           PIC  9(002).

       01  WRK-DATA-0025EC             PIC  9(009) COMP-3 VALUE ZEROS.

       01  WRK-DATA-0025S              PIC  9(009) VALUE ZEROS.
       01  WRK-DATA-0025E-R            REDEFINES WRK-DATA-0025S.
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-0025S           PIC  9(004).
           05  WRK-MES-0025S           PIC  9(002).
           05  WRK-DIA-0025S           PIC  9(002).

       01  WRK-DATA-0025SC             PIC  9(009) COMP-3 VALUE ZEROS.

       01  WRK-DATA-ATU-TRAT.
           05  WRK-DIA-DB2             PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004) VALUE ZEROS.

       01  WRK-DPROTRAT-ATU-INV        PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROTRAT-ATU-INV-R      REDEFINES WRK-DPROTRAT-ATU-INV.
           05  WRK-ANOT-ATU-INV        PIC  9(004).
           05  WRK-MEST-ATU-INV        PIC  9(002).
           05  WRK-DIAT-ATU-INV        PIC  9(002).

       01  WRK-PROCM-PROX01.
           05  WRK-DIA-TRAT-PROX       PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES-TRAT-PROX       PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO-TRAT-PROX       PIC  9(004) VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA                 PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES                 PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO                 PIC  9(004) VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-DINIC-INV               PIC  9(008)         VALUE ZEROS.
       01  WRK-DINIC-INV-R             REDEFINES WRK-DINIC-INV.
           05  WRK-IANO-INV            PIC  9(004).
           05  WRK-IMES-INV            PIC  9(002).
           05  WRK-IDIA-INV            PIC  9(002).

       01  WRK-DFIM-INV               PIC  9(008)         VALUE ZEROS.
       01  WRK-DFIM-INV-R             REDEFINES WRK-DFIM-INV.
           05  WRK-FANO-INV            PIC  9(004).
           05  WRK-FMES-INV            PIC  9(002).
           05  WRK-FDIA-INV            PIC  9(002).

       01  WRK-FIM-CURSOR              PIC  X(001)        VALUE SPACES.
151209 01  WRK-FIM-CURSOR2             PIC  X(001)        VALUE SPACES.

080512 01  WRK-ATU-INSERT.
080512     05  WRK-DIA-INSERT          PIC  9(002) VALUE ZEROS.
080512     05  FILLER                  PIC  X(001) VALUE '.'.
080512     05  WRK-MES-INSERT          PIC  9(002) VALUE ZEROS.
080512     05  FILLER                  PIC  X(001) VALUE '.'.
080512     05  WRK-ANO-INSERT          PIC  9(004) VALUE ZEROS.

      *----------------------------------------------------------------*
      *    CONTROLE DE CHECKPOINT/RESTART                              *
      *----------------------------------------------------------------*

       01  FILLER                      PIC X(050) VALUE
           '* CONTROLE DE CHECKPOINT/RESTART *'.

       01  WRK-CKRS0100                PIC  X(008)         VALUE
           'CKRS0100'.

       COPY 'I#CKRS01'.

       01  WRK-AREA-RESTART.
           05  WRK-CKP-LIDOS-CANADEEN      PIC  9(007)   VALUE ZEROS.
080512     05  WRK-CKP-INSERIDOS           PIC  9(007)   VALUE ZEROS.
           05  WRK-CKP-CANCELADOS          PIC  9(007)   VALUE ZEROS.
           05  WRK-CKP-ADES-NAO-ENCONTR    PIC  9(007)   VALUE ZEROS.
           05  WRK-CKP-ERROS               PIC  9(007)   VALUE ZEROS.
           05  WRK-CKP-ADES-ENCONTR-PAS    PIC  9(007)   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA TO FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-FS-CANADEEN             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-CANADESA             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  MENSAGENS            *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(007)         VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA BRAD0025'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ENTRADA            PIC   9(009) COMP-3  VALUE ZEROS.
       01  WRK-NUMERO-DIAS             PIC  S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-SAIDA              PIC   9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*       AREA TO BRAD7100    *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

151209     EXEC SQL
151209         INCLUDE GFCTB0M2
151209     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR-GFCTB009 CURSOR FOR
             SELECT CJUNC_DEPDC,
                    CCTA_CLI,
                    CSERVC_TARIF,
                    HINCL_REG,
                    CFUNC_MANUT_INCL,
                    CFUNC_MANUT,
                    DINIC_ADSAO_INDVD,
                    DFIM_ADSAO_INDVD,
                    HMANUT_REG,
                    CINDCD_EXCL_REG
              FROM  DB2PRD.ADSAO_INDVD_PCOTE
              WHERE CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC
                AND CCTA_CLI            = :GFCTB009.CCTA-CLI
                AND CSERVC_TARIF        = :GFCTB009.CSERVC-TARIF
                AND DFIM_ADSAO_INDVD   >= :GFCTB009.DFIM-ADSAO-INDVD
                AND CINDCD_EXCL_REG     = :GFCTB009.CINDCD-EXCL-REG
           END-EXEC.

151209     EXEC SQL
151209            DECLARE CSR02-GFCTB0M2 CURSOR WITH HOLD FOR
151209             SELECT
151209                    NSEQ_REG_CLULR  ,
151209                    COPER_FONE_MOVEL,
151209                    CDDD_FONE_MOVEL ,
022012                    NLIN_TFONI
151209               FROM DB2PRD.TADSAO_INDVD_CLULR
151209              WHERE
151209                    CAG_BCRIA    = :GFCTB0M2.CAG-BCRIA
151209                AND CCTA_CLI     = :GFCTB0M2.CCTA-CLI
151209                AND CSERVC_TARIF = :GFCTB0M2.CSERVC-TARIF
151209                AND HINCL_REG    = :GFCTB0M2.HINCL-REG
151209           ORDER BY NSEQ_REG_CLULR  DESC
151209     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*    FIM DA WORKING GFCT2109   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM 0300-INICIALIZAR-PROGRAMA.

           PERFORM 0400-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR UNTIL
                        WRK-CANADEEN  EQUAL HIGH-VALUES.

           PERFORM 9000-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0300-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           PERFORM 0500-INICIALIZAR-CHECKPOINT.

           PERFORM 0700-ACESSAR-DPROCM.

           OPEN INPUT   CANADEEN
                OUTPUT  CANADESA.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0900-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VERIFICAR VAZIO DO ARQUIVO CANADEEN.                        *
      *----------------------------------------------------------------*
       0400-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0600-LER-CANADEEN.

           IF  WRK-CANADEEN            EQUAL HIGH-VALUES
               DISPLAY '********* GFCT2109 *********'
               DISPLAY '*                          *'
               DISPLAY '*  ARQUIVO CANADEEN VAZIO  *'
               DISPLAY '*                          *'
               DISPLAY '* PROCESSAMENTO  ENCERRADO *'
               DISPLAY '*                          *'
               DISPLAY '********* GFCT2109 *********'
               PERFORM 9000-PROCEDIMENTOS-FINAIS
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INICIALIZA PROCESSO DE CHECKPOINT/RESTART                   *
      *----------------------------------------------------------------*
       0500-INICIALIZAR-CHECKPOINT     SECTION.
      *----------------------------------------------------------------*

      *--- INICIALIZA CHECKPOINT RESTART DB2

           MOVE SPACES                 TO CK01-PLAN.
           MOVE 'I'                    TO CK01-FUNCAO.
           MOVE 'DB2 '                 TO CK01-ID-DB2.
           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.

           PERFORM 9300-ROTINA-CHECKPONT-RESTART.

           IF  CK01-STATUS             EQUAL 'REST'
               MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                             TO WRK-AREA-RESTART
               MOVE WRK-CKP-LIDOS-CANADEEN   TO WRK-ACU-LIDOS-CANADEEN
080512         MOVE WRK-CKP-INSERIDOS        TO WRK-ACU-INSERIDOS
               MOVE WRK-CKP-CANCELADOS       TO WRK-ACU-CANCELADOS
               MOVE WRK-CKP-ADES-NAO-ENCONTR TO WRK-ACU-ADES-NAO-ENCONTR
               MOVE WRK-CKP-ERROS            TO WRK-ACU-ERROS
               MOVE WRK-CKP-ADES-ENCONTR-PAS TO WRK-ACU-ADES-ENCONTR-PAS
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA LEITURA DO ARQUIVO CANADEEN.                    *
      *----------------------------------------------------------------*
       0600-LER-CANADEEN               SECTION.
      *----------------------------------------------------------------*

           READ CANADEEN.

           IF  WRK-FS-CANADEEN         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CANADEEN
               GO                      TO 0600-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0950-TESTAR-FS-CANADEEN.

           MOVE GFCTC8-AGENCIA         OF CANADEEN
                                       TO WRK-AGENCIA-ENTR.

           MOVE GFCTC8-CONTA           OF CANADEEN
                                       TO WRK-CONTA-ENTR.

           MOVE GFCTC8-TARIFA-GFCT     OF CANADEEN
                                       TO WRK-TARIFA-GFCT-ENTR.

           MOVE GFCTC8-TARIFA-LEGADO   OF CANADEEN
                                       TO WRK-TARIFA-LEGADO-ENTR.

           MOVE GFCTC8-CENTRO-CUSTO    OF CANADEEN
                                       TO WRK-CENTRO-CUSTO-ENTR.

           MOVE GFCTC8-VALOR           OF CANADEEN
                                       TO WRK-VALOR-ENTR.

           ADD 1                       TO WRK-ACU-LIDOS-CANADEEN.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE LE TABELA GFCTBA1 E BUSCA DATA PROCM             *
      ******************************************************************
      *----------------------------------------------------------------*
       0700-ACESSAR-DPROCM             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO  :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE  SQLCODE          TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE  SPACES           TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1      TO WRK-DPROCM-ATU.
           MOVE DPROCM-PROX  OF GFCTB0A1      TO WRK-DPROCM-PROX.

           MOVE WRK-DPROCM-ATU                TO WRK-DATA-DB2
           MOVE WRK-DIA                       TO WRK-DIA-ATU-INV.
           MOVE WRK-MES                       TO WRK-MES-ATU-INV.
           MOVE WRK-ANO                       TO WRK-ANO-ATU-INV.

      ** TRATAMENTO A DATA PROCM-ANTER PARA OBTER O ULTIMO DIA DO MES **

           MOVE WRK-DIA-ATU                 TO WRK-DIA-0025E.
           MOVE WRK-MES-ATU                 TO WRK-MES-0025E.
           MOVE WRK-ANO-ATU                 TO WRK-ANO-0025E.

           INITIALIZE WRK-DATA-SAIDA
                      WRK-DATA-ENTRADA.

           MOVE 01 TO WRK-DIA-0025E

           IF WRK-MES-0025E EQUAL 12
               ADD  1  TO WRK-ANO-0025E
               MOVE 01 TO WRK-MES-0025E
           ELSE
               ADD 1 TO WRK-MES-0025E
           END-IF.

           MOVE WRK-DATA-0025E         TO WRK-DATA-0025EC.
           MOVE WRK-DATA-0025EC        TO WRK-DATA-ENTRADA.
           MOVE -01                    TO WRK-NUMERO-DIAS.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'ERRO NA BRAD0025 - DADOS INCONSISTENTES'
                                       TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-DATA-0025SC.
           MOVE WRK-DATA-0025SC        TO WRK-DATA-0025S.
           MOVE WRK-DIA-0025S          TO WRK-DIA-DB2.
           MOVE WRK-MES-0025S          TO WRK-MES-DB2.
           MOVE WRK-ANO-0025S          TO WRK-ANO-DB2.

           MOVE WRK-DATA-ATU-TRAT             TO WRK-DATA-DB2.
           MOVE WRK-DIA                       TO WRK-DIAT-ATU-INV.
           MOVE WRK-MES                       TO WRK-MEST-ATU-INV.
           MOVE WRK-ANO                       TO WRK-ANOT-ATU-INV.

      ** TRATAMENTO A DATA PROCM-ATUAL PARA OBTER O MES SEGUINTE COM O
      *  PRIMEIRO DIA DO MES *******************************************

080512     MOVE WRK-DPROCM-ATU                TO WRK-ATU-INSERT.
080512     MOVE 01                            TO WRK-DIA-INSERT.

080512     IF WRK-MES-INSERT EQUAL 12
080512         ADD  1  TO WRK-ANO-INSERT
080512         MOVE 01 TO WRK-MES-INSERT
080512     ELSE
080512         ADD 1 TO WRK-MES-INSERT
080512     END-IF.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS                                      *
      *----------------------------------------------------------------*
       0900-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0950-TESTAR-FS-CANADEEN.
           PERFORM 0960-TESTAR-FS-CANADESA.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO CANADEEN                    *
      *----------------------------------------------------------------*
       0950-TESTAR-FS-CANADEEN           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CANADEEN         NOT EQUAL '00'
               MOVE 'CANADEEN'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CANADEEN    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO CANADESA                    *
      *----------------------------------------------------------------*
       0960-TESTAR-FS-CANADESA           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CANADESA         NOT EQUAL '00'
               MOVE 'CANADESA'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-CANADESA    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0960-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL.                                    *
      *----------------------------------------------------------------*
       1000-PROCESSAR                        SECTION.
      *----------------------------------------------------------------*

      *    ADD   1                     TO  ACU-CHECKPOINT-RESTART.
      *
      *    IF  ACU-CHECKPOINT-RESTART  EQUAL  08
      *        MOVE  'APL'             TO  ERR-TIPO-ACESSO
      *        MOVE  'ERRO FORCADO P/ TESTE DA ROTINA CHECKPOINT/RESTART
      *-             ', APOS 7 REGS PROCESSADOS'
      *                                TO  ERR-TEXTO
      *
      *        PERFORM 9999-ROTINA-ERRO
      *    END-IF.

           IF (WRK-AGENCIA-ENTR           NOT NUMERIC  OR
               WRK-AGENCIA-ENTR           EQUAL ZEROS) OR
              (WRK-CONTA-ENTR             NOT NUMERIC  OR
               WRK-CONTA-ENTR             EQUAL ZEROS) OR
              (WRK-TARIFA-GFCT-ENTR       NOT NUMERIC  OR
               WRK-TARIFA-GFCT-ENTR       EQUAL ZEROS)
              MOVE WRK-AGENCIA-ENTR       TO  WRK-AGENCIA-SAID
              MOVE WRK-CONTA-ENTR         TO  WRK-CONTA-SAID
              MOVE WRK-TARIFA-GFCT-ENTR   TO  WRK-TARIFA-GFCT-SAID
              MOVE WRK-TARIFA-LEGADO-ENTR TO  WRK-TARIFA-LEGADO-SAID
              MOVE WRK-CENTRO-CUSTO-ENTR  TO  WRK-CENTRO-CUSTO-SAID
              MOVE ZEROS                  TO  WRK-DATA-INIC-VIG-SAID
              MOVE ZEROS                  TO  WRK-DATA-FIM-VIG-SAID
              MOVE WRK-VALOR-ENTR         TO  WRK-VALOR-SAID
              MOVE 'CAMPOS INCONSISTENTES' TO WRK-MENSAGEM-SAID
              MOVE 1                      TO  WRK-IND-ERRO-SAID
              ADD  1                      TO  WRK-ACU-ERROS

              PERFORM 2200-GRAVAR-CANADESA
           ELSE
              PERFORM 1100-ACESSAR-GFCTB009
           END-IF.

           MOVE 'P'                    TO CK01-FUNCAO.
           MOVE 'DB2'                  TO CK01-ID-DB2.

           MOVE WRK-ACU-LIDOS-CANADEEN   TO WRK-CKP-LIDOS-CANADEEN.
           MOVE WRK-ACU-INSERIDOS        TO WRK-CKP-INSERIDOS.
           MOVE WRK-ACU-CANCELADOS       TO WRK-CKP-CANCELADOS.
           MOVE WRK-ACU-ADES-NAO-ENCONTR TO WRK-CKP-ADES-NAO-ENCONTR.
           MOVE WRK-ACU-ERROS            TO WRK-CKP-ERROS.
           MOVE WRK-ACU-ADES-ENCONTR-PAS TO WRK-CKP-ADES-ENCONTR-PAS.

           MOVE LENGTH OF WRK-AREA-RESTART
                                       TO CK01-TAM-AREA-RESTART.
           MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.

           PERFORM 9300-ROTINA-CHECKPONT-RESTART.

           PERFORM 0600-LER-CANADEEN.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-ACESSAR-GFCTB009       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-ATU-TRAT        TO DFIM-ADSAO-INDVD OF GFCTB009
           MOVE ZEROS                    TO CINDCD-EXCL-REG  OF GFCTB009
           MOVE WRK-AGENCIA-ENTR         TO CJUNC-DEPDC      OF GFCTB009
           MOVE WRK-CONTA-ENTR           TO CCTA-CLI         OF GFCTB009
           MOVE WRK-TARIFA-GFCT-ENTR     TO CSERVC-TARIF     OF GFCTB009

           EXEC SQL

             SELECT CJUNC_DEPDC,
                    CCTA_CLI,
                    CSERVC_TARIF,
                    HINCL_REG,
                    CFUNC_MANUT_INCL,
                    CFUNC_MANUT,
                    DINIC_ADSAO_INDVD,
                    DFIM_ADSAO_INDVD,
                    HMANUT_REG,
                    CINDCD_EXCL_REG
              INTO :GFCTB009.CJUNC-DEPDC,
                   :GFCTB009.CCTA-CLI,
                   :GFCTB009.CSERVC-TARIF,
                   :GFCTB009.HINCL-REG,
                   :GFCTB009.CFUNC-MANUT-INCL,
                   :GFCTB009.CFUNC-MANUT,
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD,
                   :GFCTB009.HMANUT-REG,
                   :GFCTB009.CINDCD-EXCL-REG
              FROM  DB2PRD.ADSAO_INDVD_PCOTE
              WHERE CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC
                AND CCTA_CLI            = :GFCTB009.CCTA-CLI
                AND CSERVC_TARIF        = :GFCTB009.CSERVC-TARIF
                AND DFIM_ADSAO_INDVD   >= :GFCTB009.DFIM-ADSAO-INDVD
                AND CINDCD_EXCL_REG     = :GFCTB009.CINDCD-EXCL-REG

           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100
                                        AND -811) OR
              (SQLWARN0                 EQUAL 'W'      )
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_INDVD_PCOTE' TO ERR-DBD-TAB
               MOVE 'SELECT  '          TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE  '0020'             TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE              EQUAL +100
               MOVE WRK-AGENCIA-ENTR       TO  WRK-AGENCIA-SAID
               MOVE WRK-CONTA-ENTR         TO  WRK-CONTA-SAID
               MOVE WRK-TARIFA-GFCT-ENTR   TO  WRK-TARIFA-GFCT-SAID
               MOVE WRK-TARIFA-LEGADO-ENTR TO  WRK-TARIFA-LEGADO-SAID
               MOVE WRK-CENTRO-CUSTO-ENTR  TO  WRK-CENTRO-CUSTO-SAID
               MOVE ZEROS                  TO  WRK-DATA-INIC-VIG-SAID
               MOVE ZEROS                  TO  WRK-DATA-FIM-VIG-SAID
               MOVE WRK-VALOR-ENTR         TO  WRK-VALOR-SAID
               MOVE 'ADESAO NAO ENCONTRADA' TO WRK-MENSAGEM-SAID
               MOVE 1               TO  WRK-IND-ERRO-SAID
               ADD  1               TO  WRK-ACU-ADES-NAO-ENCONTR
               PERFORM 2200-GRAVAR-CANADESA
           ELSE
               IF  SQLCODE              EQUAL -811
                   PERFORM 1150-TRATA-CURSOR
               ELSE
                   MOVE DINIC-ADSAO-INDVD     OF GFCTB009
                                              TO WRK-DINIC-ADSAO-INDVD
                   MOVE DFIM-ADSAO-INDVD      OF GFCTB009
                                              TO WRK-DFIM-ADSAO-INDVD

                   MOVE WRK-DINIC-ADSAO-INDVD TO WRK-DATA-DB2
                   MOVE WRK-DIA               TO WRK-IDIA-INV
                   MOVE WRK-MES               TO WRK-IMES-INV
                   MOVE WRK-ANO               TO WRK-IANO-INV

                   MOVE WRK-DFIM-ADSAO-INDVD  TO WRK-DATA-DB2
                   MOVE WRK-DIA               TO WRK-FDIA-INV
                   MOVE WRK-MES               TO WRK-FMES-INV
                   MOVE WRK-ANO               TO WRK-FANO-INV

                   MOVE CJUNC-DEPDC           OF GFCTB009
                                              TO WRK-CJUNC-DEPDC
                   MOVE CCTA-CLI              OF GFCTB009
                                              TO WRK-CCTA-CLI
                   MOVE CSERVC-TARIF          OF GFCTB009
                                              TO WRK-CSERVC-TARIF
                   MOVE HINCL-REG             OF GFCTB009
                                              TO WRK-HINCL-REG
                   PERFORM 1200-UPDATE-GFCTB009
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-TRATA-CURSOR                       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1152-ABRE-CURSOR.

           MOVE    'N'                 TO  WRK-FIM-CURSOR.

           PERFORM 1154-LE-CURSOR
                   UNTIL WRK-FIM-CURSOR = 'S'

           PERFORM 1156-FECHA-CURSOR.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1152-ABRE-CURSOR                       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-ATU-TRAT        TO DFIM-ADSAO-INDVD
                                         OF GFCTB009.
           MOVE ZEROS                    TO CINDCD-EXCL-REG
                                         OF GFCTB009.
           MOVE WRK-AGENCIA-ENTR         TO CJUNC-DEPDC
                                         OF GFCTB009.
           MOVE WRK-CONTA-ENTR           TO CCTA-CLI
                                         OF GFCTB009.
           MOVE WRK-TARIFA-GFCT-ENTR     TO CSERVC-TARIF
                                         OF GFCTB009.

           EXEC SQL
               OPEN CSR-GFCTB009
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
                MOVE 'DB2'               TO ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE' TO ERR-DBD-TAB
                MOVE 'OPEN    '          TO ERR-FUN-COMANDO
                MOVE  SQLCODE            TO ERR-SQL-CODE
                MOVE  '0030'             TO ERR-LOCAL
                MOVE  SPACES             TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1152-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1154-LE-CURSOR                         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-GFCTB009
               INTO :GFCTB009.CJUNC-DEPDC,
                    :GFCTB009.CCTA-CLI,
                    :GFCTB009.CSERVC-TARIF,
                    :GFCTB009.HINCL-REG,
                    :GFCTB009.CFUNC-MANUT-INCL,
                    :GFCTB009.CFUNC-MANUT,
                    :GFCTB009.DINIC-ADSAO-INDVD,
                    :GFCTB009.DFIM-ADSAO-INDVD,
                    :GFCTB009.HMANUT-REG,
                    :GFCTB009.CINDCD-EXCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_INDVD_PCOTE' TO ERR-DBD-TAB
               MOVE 'FETCH   '          TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE  '0040'             TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL  ZEROS
               MOVE DINIC-ADSAO-INDVD  OF  GFCTB009
                                       TO  WRK-DINIC-ADSAO-INDVD
               MOVE DFIM-ADSAO-INDVD   OF  GFCTB009
                                       TO  WRK-DFIM-ADSAO-INDVD

               MOVE WRK-DINIC-ADSAO-INDVD  TO WRK-DATA-DB2
               MOVE WRK-DIA                TO WRK-IDIA-INV
               MOVE WRK-MES                TO WRK-IMES-INV
               MOVE WRK-ANO                TO WRK-IANO-INV

               MOVE WRK-DFIM-ADSAO-INDVD   TO WRK-DATA-DB2
               MOVE WRK-DIA                TO WRK-FDIA-INV
               MOVE WRK-MES                TO WRK-FMES-INV
               MOVE WRK-ANO                TO WRK-FANO-INV

               MOVE CJUNC-DEPDC        OF  GFCTB009
                                       TO  WRK-CJUNC-DEPDC
               MOVE CCTA-CLI               OF GFCTB009
                                       TO  WRK-CCTA-CLI
               MOVE CSERVC-TARIF           OF GFCTB009
                                       TO  WRK-CSERVC-TARIF
               MOVE HINCL-REG              OF GFCTB009
                                       TO  WRK-HINCL-REG
               PERFORM 1200-UPDATE-GFCTB009
           ELSE
               MOVE    'S'             TO     WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       1154-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1156-FECHA-CURSOR                      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB009
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'               TO ERR-TIPO-ACESSO
               MOVE 'ADSAO_INDVD_PCOTE' TO ERR-DBD-TAB
               MOVE 'FETCH   '          TO ERR-FUN-COMANDO
               MOVE  SQLCODE            TO ERR-SQL-CODE
               MOVE  '0050'             TO ERR-LOCAL
               MOVE  SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1156-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-UPDATE-GFCTB009           SECTION.
      *----------------------------------------------------------------*

080512     IF WRK-DFIM-INV  EQUAL  WRK-DPROTRAT-ATU-INV
               MOVE WRK-AGENCIA-ENTR       TO  WRK-AGENCIA-SAID
               MOVE WRK-CONTA-ENTR         TO  WRK-CONTA-SAID
               MOVE WRK-TARIFA-GFCT-ENTR   TO  WRK-TARIFA-GFCT-SAID
               MOVE WRK-TARIFA-LEGADO-ENTR TO  WRK-TARIFA-LEGADO-SAID
               MOVE WRK-CENTRO-CUSTO-ENTR  TO  WRK-CENTRO-CUSTO-SAID
               MOVE ZEROS                  TO  WRK-DATA-INIC-VIG-SAID
               MOVE ZEROS                  TO  WRK-DATA-FIM-VIG-SAID
               MOVE WRK-VALOR-ENTR         TO  WRK-VALOR-SAID
               MOVE 'ADESAO COM VIGENCIA PASSADA - NAO CANCELADA'
                                           TO WRK-MENSAGEM-SAID
               MOVE 1               TO  WRK-IND-ERRO-SAID
               ADD 1 TO WRK-ACU-ADES-ENCONTR-PAS
               PERFORM 2200-GRAVAR-CANADESA
080512     ELSE
080512         IF WRK-DINIC-INV GREATER WRK-DPROTRAT-ATU-INV
                   PERFORM 1900-UPDATE-GFCTB009-FUTURA
080512         ELSE
                  PERFORM 1800-UPDATE-GFCTB009-VIGENTE
080512         END-IF
080512     END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1800-UPDATE-GFCTB009-VIGENTE            SECTION.
      *----------------------------------------------------------------*

080512     PERFORM 2000-INSERT-TABELA-GFCTB009.

           MOVE WRK-DATA-ATU-TRAT TO DFIM-ADSAO-INDVD  OF GFCTB009
           MOVE 0                 TO CINDCD-EXCL-REG   OF GFCTB009
           MOVE 2109              TO CFUNC-MANUT       OF GFCTB009
           MOVE WRK-CJUNC-DEPDC   TO CJUNC-DEPDC       OF GFCTB009
           MOVE WRK-CCTA-CLI      TO CCTA-CLI          OF GFCTB009
           MOVE WRK-CSERVC-TARIF  TO CSERVC-TARIF      OF GFCTB009
           MOVE WRK-HINCL-REG     TO HINCL-REG         OF GFCTB009

           EXEC SQL
                UPDATE  DB2PRD.ADSAO_INDVD_PCOTE
                 SET  DFIM_ADSAO_INDVD = :GFCTB009.DFIM-ADSAO-INDVD,
                      CINDCD_EXCL_REG  = :GFCTB009.CINDCD-EXCL-REG,
                      CFUNC_MANUT      = :GFCTB009.CFUNC-MANUT,
                      HMANUT_REG       =  CURRENT TIMESTAMP
                 WHERE  CJUNC_DEPDC    = :GFCTB009.CJUNC-DEPDC
                   AND  CCTA_CLI       = :GFCTB009.CCTA-CLI
                   AND  CSERVC_TARIF   = :GFCTB009.CSERVC-TARIF
                   AND  HINCL_REG      = :GFCTB009.HINCL-REG
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE'
                                       TO ERR-DBD-TAB
                MOVE 'UPDATE'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0060'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM

                PERFORM 9999-ROTINA-ERRO
           END-IF.

151209     PERFORM 2050-ULTIMA-SEQUENCIA-GFCTB0M2.
151209     IF  WRK-FIM-CURSOR2 EQUAL 'N'
151209         PERFORM 2100-INSERT-TABELA-GFCTB0M2
151209         PERFORM 2150-UPDATE-GFCTB0M2
151209     END-IF.

           MOVE WRK-AGENCIA-ENTR       TO  WRK-AGENCIA-SAID
           MOVE WRK-CONTA-ENTR         TO  WRK-CONTA-SAID
           MOVE WRK-TARIFA-GFCT-ENTR   TO  WRK-TARIFA-GFCT-SAID
           MOVE WRK-TARIFA-LEGADO-ENTR TO  WRK-TARIFA-LEGADO-SAID
           MOVE WRK-CENTRO-CUSTO-ENTR  TO  WRK-CENTRO-CUSTO-SAID
           MOVE WRK-DINIC-INV          TO  WRK-DATA-INIC-VIG-SAID
           MOVE WRK-DFIM-INV           TO  WRK-DATA-FIM-VIG-SAID
           MOVE WRK-VALOR-ENTR         TO  WRK-VALOR-SAID
           ADD  1                      TO WRK-ACU-CANCELADOS.
           MOVE 'CANCELAMENTO EFETUADO COM SUCESSO' TO WRK-MENSAGEM-SAID
           MOVE 0                      TO  WRK-IND-ERRO-SAID

           PERFORM 2200-GRAVAR-CANADESA.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1900-UPDATE-GFCTB009-FUTURA             SECTION.
      *----------------------------------------------------------------*

           MOVE 1                 TO CINDCD-EXCL-REG   OF GFCTB009
           MOVE 2109              TO CFUNC-MANUT       OF GFCTB009
           MOVE WRK-CJUNC-DEPDC   TO CJUNC-DEPDC       OF GFCTB009
           MOVE WRK-CCTA-CLI      TO CCTA-CLI          OF GFCTB009
           MOVE WRK-CSERVC-TARIF  TO CSERVC-TARIF      OF GFCTB009
           MOVE WRK-HINCL-REG     TO HINCL-REG         OF GFCTB009

           EXEC SQL
                UPDATE  DB2PRD.ADSAO_INDVD_PCOTE
                SET   CINDCD_EXCL_REG  = :GFCTB009.CINDCD-EXCL-REG,
                      CFUNC_MANUT      = :GFCTB009.CFUNC-MANUT,
                      HMANUT_REG       =  CURRENT TIMESTAMP
                 WHERE  CJUNC_DEPDC    = :GFCTB009.CJUNC-DEPDC
                   AND  CCTA_CLI       = :GFCTB009.CCTA-CLI
                   AND  CSERVC_TARIF   = :GFCTB009.CSERVC-TARIF
                   AND  HINCL_REG      = :GFCTB009.HINCL-REG
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE'
                                       TO ERR-DBD-TAB
                MOVE 'UPDATE'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0070'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM

                PERFORM 9999-ROTINA-ERRO
           END-IF.

151209     PERFORM 2050-ULTIMA-SEQUENCIA-GFCTB0M2.
151209     IF  WRK-FIM-CURSOR2 EQUAL 'N'
151209         PERFORM 2150-UPDATE-GFCTB0M2
151209     END-IF.

           MOVE WRK-AGENCIA-ENTR       TO  WRK-AGENCIA-SAID
           MOVE WRK-CONTA-ENTR         TO  WRK-CONTA-SAID
           MOVE WRK-TARIFA-GFCT-ENTR   TO  WRK-TARIFA-GFCT-SAID
           MOVE WRK-TARIFA-LEGADO-ENTR TO  WRK-TARIFA-LEGADO-SAID
           MOVE WRK-CENTRO-CUSTO-ENTR  TO  WRK-CENTRO-CUSTO-SAID
           MOVE WRK-DINIC-INV          TO  WRK-DATA-INIC-VIG-SAID
           MOVE WRK-DFIM-INV           TO  WRK-DATA-FIM-VIG-SAID
           MOVE WRK-VALOR-ENTR         TO  WRK-VALOR-SAID
           ADD  1                      TO WRK-ACU-CANCELADOS.
           MOVE 'CANCELAMENTO EFETUADO COM SUCESSO' TO WRK-MENSAGEM-SAID
           MOVE 0                      TO  WRK-IND-ERRO-SAID

           PERFORM 2200-GRAVAR-CANADESA.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

080512*----------------------------------------------------------------*
080512 2000-INSERT-TABELA-GFCTB009            SECTION.
080512*----------------------------------------------------------------*
080512
151209     EXEC SQL
151209          SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
151209     END-EXEC.

080512     MOVE WRK-CJUNC-DEPDC   TO CJUNC-DEPDC       OF GFCTB009.
080512     MOVE WRK-CCTA-CLI      TO CCTA-CLI          OF GFCTB009.
080512     MOVE WRK-CSERVC-TARIF  TO CSERVC-TARIF      OF GFCTB009.
080512     MOVE 2109              TO CFUNC-MANUT-INCL  OF GFCTB009.
080512     MOVE 2109              TO CFUNC-MANUT       OF GFCTB009.
080512     MOVE WRK-ATU-INSERT    TO DINIC-ADSAO-INDVD OF GFCTB009.
080512     MOVE 1                 TO CINDCD-EXCL-REG   OF GFCTB009.
080512
080512     EXEC SQL
080512          INSERT
080512            INTO   DB2PRD.ADSAO_INDVD_PCOTE
080512                  (CJUNC_DEPDC      ,
080512                   CCTA_CLI         ,
080512                   CSERVC_TARIF     ,
080512                   HINCL_REG        ,
080512                   CFUNC_MANUT_INCL ,
080512                   CFUNC_MANUT      ,
080512                   DINIC_ADSAO_INDVD,
080512                   DFIM_ADSAO_INDVD ,
080512                   HMANUT_REG       ,
080512                   CINDCD_EXCL_REG)
080512          VALUES (:GFCTB009.CJUNC-DEPDC      ,
080512                  :GFCTB009.CCTA-CLI         ,
080512                  :GFCTB009.CSERVC-TARIF     ,
151209                  :WRK-TIMESTAMP             ,
080512                  :GFCTB009.CFUNC-MANUT-INCL ,
080512                  :GFCTB009.CFUNC-MANUT      ,
080512                  :GFCTB009.DINIC-ADSAO-INDVD,
080512                  :GFCTB009.DFIM-ADSAO-INDVD ,
080512                   CURRENT TIMESTAMP         ,
080512                  :GFCTB009.CINDCD-EXCL-REG)
080512     END-EXEC.
080512
080512     IF  (SQLCODE                NOT EQUAL ZEROS) OR
080512         (SQLWARN0               EQUAL 'W')
080512          MOVE 'DB2'             TO ERR-TIPO-ACESSO
080512          MOVE 'ADSAO_INDVD_PCOTE'
080512                                 TO ERR-DBD-TAB
080512          MOVE 'INSERT'          TO ERR-FUN-COMANDO
080512          MOVE SQLCODE           TO ERR-SQL-CODE
080512          MOVE '0080'            TO ERR-LOCAL
080512          MOVE SPACES            TO ERR-SEGM
080512
080512          PERFORM 9999-ROTINA-ERRO
080512     END-IF.
080512
080512     ADD  1                  TO WRK-ACU-INSERIDOS.
080512
080512*----------------------------------------------------------------*
080512 2000-99-FIM.                    EXIT.
080512*----------------------------------------------------------------*
151209*----------------------------------------------------------------*
151209 2050-ULTIMA-SEQUENCIA-GFCTB0M2  SECTION.
151209*----------------------------------------------------------------*
151209
151209     MOVE WRK-CJUNC-DEPDC  TO CAG-BCRIA     OF GFCTB0M2.
151209     MOVE WRK-CCTA-CLI     TO CCTA-CLI      OF GFCTB0M2.
151209     MOVE WRK-CSERVC-TARIF TO CSERVC-TARIF  OF GFCTB0M2.
151209     MOVE WRK-HINCL-REG    TO HINCL-REG     OF GFCTB0M2.
151209
151209     EXEC SQL
151209             OPEN  CSR02-GFCTB0M2
151209     END-EXEC.
151209
151209     IF (SQLCODE                 NOT EQUAL ZEROS)
151209     OR (SQLWARN0                EQUAL 'W'      )
151209         MOVE 'DB2'              TO ERR-TIPO-ACESSO
151209         MOVE 'TADSAO_INDVD_CLULR'
151209                                 TO ERR-DBD-TAB
151209         MOVE 'OPEN'             TO ERR-FUN-COMANDO
151209         MOVE  SQLCODE           TO ERR-SQL-CODE
151209         MOVE '0090'             TO ERR-LOCAL
151209         MOVE  SPACES            TO ERR-SEGM
151209         PERFORM 9999-ROTINA-ERRO
151209     END-IF.
151209
151209     EXEC SQL
151209          FETCH  CSR02-GFCTB0M2
151209           INTO :GFCTB0M2.NSEQ-REG-CLULR  ,
151209                :GFCTB0M2.COPER-FONE-MOVEL,
151209                :GFCTB0M2.CDDD-FONE-MOVEL ,
022012                :GFCTB0M2.NLIN-TFONI
151209     END-EXEC.
151209
151209     IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
151209     OR (SQLWARN0                EQUAL 'W'               )
151209         MOVE 'DB2'              TO ERR-TIPO-ACESSO
151209         MOVE 'TADSAO_INDVD_CLULR'
151209                                 TO ERR-DBD-TAB
151209         MOVE 'FETCH'            TO ERR-FUN-COMANDO
151209         MOVE  SQLCODE           TO ERR-SQL-CODE
151209         MOVE '0100'             TO ERR-LOCAL
151209         MOVE  SPACES            TO ERR-SEGM
151209         PERFORM 9999-ROTINA-ERRO
151209     END-IF.
151209
151209     IF  SQLCODE                NOT EQUAL ZEROS
151209         MOVE 'S'                TO WRK-FIM-CURSOR2
151209         ADD   1                 TO WRK-ACU-SEM-GFCTB0M2
151209     ELSE
151209         MOVE  NSEQ-REG-CLULR    OF GFCTB0M2
151209                                 TO WRK-NSEQ-REG-CLULR
151209         MOVE 'N'                TO WRK-FIM-CURSOR2
151209     END-IF.
151209
151209     EXEC SQL
151209          CLOSE CSR02-GFCTB0M2
151209     END-EXEC.
151209
151209     IF (SQLCODE                 NOT EQUAL ZEROS)
151209     OR (SQLWARN0                EQUAL 'W'      )
151209         MOVE 'DB2'              TO ERR-TIPO-ACESSO
151209         MOVE 'TADSAO_INDVD_CLULR'
151209                                 TO ERR-DBD-TAB
151209         MOVE 'CLOSE'            TO ERR-FUN-COMANDO
151209         MOVE  SQLCODE           TO ERR-SQL-CODE
151209         MOVE '0110'             TO ERR-LOCAL
151209         MOVE  SPACES            TO ERR-SEGM
151209         PERFORM 9999-ROTINA-ERRO
151209     END-IF.
151209
151209*----------------------------------------------------------------*
151209 2050-99-FIM.                    EXIT.
151209*----------------------------------------------------------------*
151209*----------------------------------------------------------------*
151209 2100-INSERT-TABELA-GFCTB0M2            SECTION.
151209*----------------------------------------------------------------*
151209
151209     MOVE WRK-CJUNC-DEPDC   TO CAG-BCRIA         OF GFCTB0M2.
151209     MOVE WRK-CCTA-CLI      TO CCTA-CLI          OF GFCTB0M2.
151209     MOVE WRK-CSERVC-TARIF  TO CSERVC-TARIF      OF GFCTB0M2.
151209     MOVE 1                 TO NSEQ-REG-CLULR    OF GFCTB0M2.
151209     MOVE 2109              TO CFUNC-INCL-SIST   OF GFCTB0M2.
151209     MOVE 2109              TO CFUNC-MANUT       OF GFCTB0M2.
151209     MOVE '0001-01-01-01.01.01.000001'
151209                            TO HMANUT-REG        OF GFCTB0M2.
151209
151209     EXEC SQL
151209          INSERT
151209            INTO   DB2PRD.TADSAO_INDVD_CLULR
151209                  (CAG_BCRIA        ,
151209                   CCTA_CLI         ,
151209                   CSERVC_TARIF     ,
151209                   HINCL_REG        ,
151209                   NSEQ_REG_CLULR   ,
151209                   COPER_FONE_MOVEL ,
151209                   CDDD_FONE_MOVEL  ,
151209                   CFUNC_INCL_SIST  ,
151209                   CFUNC_MANUT      ,
022012                   HMANUT_REG       ,
022012                   NLIN_TFONI       )
151209          VALUES (:GFCTB0M2.CAG-BCRIA        ,
151209                  :GFCTB0M2.CCTA-CLI         ,
151209                  :GFCTB0M2.CSERVC-TARIF     ,
151209                  :WRK-TIMESTAMP             ,
151209                  :GFCTB0M2.NSEQ-REG-CLULR   ,
151209                  :GFCTB0M2.COPER-FONE-MOVEL ,
151209                  :GFCTB0M2.CDDD-FONE-MOVEL  ,
151209                  :GFCTB0M2.CFUNC-INCL-SIST  ,
151209                  :GFCTB0M2.CFUNC-MANUT      ,
022012                  :GFCTB0M2.HMANUT-REG       ,
022012                  :GFCTB0M2.NLIN-TFONI       )
151209     END-EXEC.
151209
151209     IF  (SQLCODE                NOT EQUAL ZEROS) OR
151209         (SQLWARN0               EQUAL 'W')
151209          MOVE 'DB2'             TO ERR-TIPO-ACESSO
151209          MOVE 'TADSAO_INDVD_CLULR'
151209                                 TO ERR-DBD-TAB
151209          MOVE 'INSERT'          TO ERR-FUN-COMANDO
151209          MOVE SQLCODE           TO ERR-SQL-CODE
151209          MOVE '0120'            TO ERR-LOCAL
151209          MOVE SPACES            TO ERR-SEGM
151209
151209          PERFORM 9999-ROTINA-ERRO
151209     ELSE
151209          ADD   1                TO WRK-ACU-ISRT-GFCTB0M2
151209     END-IF.
151209
151209*----------------------------------------------------------------*
151209 2100-99-FIM.                    EXIT.
151209*----------------------------------------------------------------*

151209*----------------------------------------------------------------*
151209 2150-UPDATE-GFCTB0M2                    SECTION.
151209*----------------------------------------------------------------*
151209
151209     MOVE WRK-CJUNC-DEPDC    TO CAG-BCRIA         OF GFCTB0M2.
151209     MOVE WRK-CCTA-CLI       TO CCTA-CLI          OF GFCTB0M2.
151209     MOVE WRK-CSERVC-TARIF   TO CSERVC-TARIF      OF GFCTB0M2.
151209     MOVE WRK-HINCL-REG      TO HINCL-REG         OF GFCTB0M2
151209     MOVE WRK-NSEQ-REG-CLULR TO NSEQ-REG-CLULR    OF GFCTB0M2
151209     MOVE 2109               TO CFUNC-MANUT       OF GFCTB0M2.
151209
151209     EXEC SQL
151209          UPDATE  DB2PRD.TADSAO_INDVD_CLULR
151209             SET  CFUNC_MANUT    = :GFCTB0M2.CFUNC-MANUT,
151209                  HMANUT_REG     =  CURRENT TIMESTAMP
151209           WHERE  CAG_BCRIA      = :GFCTB0M2.CAG-BCRIA
151209             AND  CCTA_CLI       = :GFCTB0M2.CCTA-CLI
151209             AND  CSERVC_TARIF   = :GFCTB0M2.CSERVC-TARIF
151209             AND  HINCL_REG      = :GFCTB0M2.HINCL-REG
151209             AND  NSEQ_REG_CLULR = :GFCTB0M2.NSEQ-REG-CLULR
151209     END-EXEC.
151209
151209     IF  (SQLCODE                NOT EQUAL ZEROS) OR
151209         (SQLWARN0               EQUAL 'W')
151209          MOVE 'DB2'             TO ERR-TIPO-ACESSO
151209          MOVE 'TADSAO_INDVD_CLULR'
151209                                 TO ERR-DBD-TAB
151209          MOVE 'UPDATE'          TO ERR-FUN-COMANDO
151209          MOVE SQLCODE           TO ERR-SQL-CODE
151209          MOVE '0130'            TO ERR-LOCAL
151209          MOVE SPACES            TO ERR-SEGM
151209
151209          PERFORM 9999-ROTINA-ERRO
151209     ELSE
151209          ADD   1                TO WRK-ACU-ATLZ-GFCTB0M2
151209     END-IF.
151209
151209*----------------------------------------------------------------*
151209 2150-99-FIM.                    EXIT.
151209*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-GRAVAR-CANADESA            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AGENCIA-SAID       TO GFCTC9-AGENCIA.
           MOVE WRK-CONTA-SAID         TO GFCTC9-CONTA.
           MOVE WRK-TARIFA-GFCT-SAID   TO GFCTC9-TARIFA-GFCT.
           MOVE WRK-TARIFA-LEGADO-SAID TO GFCTC9-TARIFA-LEGADO.
           MOVE WRK-CENTRO-CUSTO-SAID  TO GFCTC9-CENTRO-CUSTO.

           MOVE WRK-DATA-INIC-VIG-SAID TO GFCTC9-DATA-INIC-VIG.
           MOVE WRK-DATA-FIM-VIG-SAID  TO GFCTC9-DATA-FIM-VIG.
           MOVE WRK-VALOR-SAID         TO GFCTC9-VALOR.
           MOVE WRK-MENSAGEM-SAID      TO GFCTC9-MENSAGEM.
           MOVE WRK-IND-ERRO-SAID      TO GFCTC9-IND-ERRO.

           WRITE GFCTC9-REG OF CANADESA.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0900-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       9000-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           CLOSE CANADEEN
                 CANADESA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0900-TESTAR-FILE-STATUS.

           PERFORM 9100-EXIBIR-DISPLAY-FINAL.

      *--- FINALIZA CHECKPOINT RESTART DB2

           MOVE 'F'                    TO CK01-FUNCAO.
           MOVE 'DB2'                  TO CK01-ID-DB2.

           PERFORM 9300-ROTINA-CHECKPONT-RESTART.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EXIBIR ESTATISTICA                                         *
      *----------------------------------------------------------------*
       9100-EXIBIR-DISPLAY-FINAL       SECTION.
      *----------------------------------------------------------------*

           DISPLAY '                                                  '.
           DISPLAY '**************************************************'.
           DISPLAY '*                                                *'.
           DISPLAY '*               PROGRAMA GFCT2109                *'.
           DISPLAY '*                                                *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.

           IF  WRK-ERRO-PROCESSO       EQUAL 'S'
               DISPLAY '*             PROCESSAMENTO ENCERRADO         '
                                                                 '   *'
           ELSE
               DISPLAY '*       PROCESSAMENTO ENCERRADO COM SUCESSO   '
                                                                 '   *'
           END-IF.

           DISPLAY '*                                                *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '*                                                *'.

           MOVE WRK-ACU-LIDOS-CANADEEN         TO WRK-MASCARA.
           DISPLAY '* QTDE REG. LIDOS                    = '
                                                       WRK-MASCARA ' *'.

080512     MOVE WRK-ACU-INSERIDOS              TO WRK-MASCARA.
080512     DISPLAY '* QTDE REG. INSERIDOS                = '
080512                                                 WRK-MASCARA ' *'.

           MOVE WRK-ACU-CANCELADOS             TO WRK-MASCARA.
           DISPLAY '* QTDE REG. CANCELADOS               = '
                                                       WRK-MASCARA ' *'.

           MOVE WRK-ACU-ADES-NAO-ENCONTR       TO WRK-MASCARA.
           DISPLAY '* QTDE ADESAO NAO ENCONTRADA         = '
                                                       WRK-MASCARA ' *'.

           MOVE WRK-ACU-ERROS                  TO WRK-MASCARA.
           DISPLAY '* QTDE REG. COM ERRO                 = '
                                                       WRK-MASCARA ' *'.

151209     MOVE WRK-ACU-ISRT-GFCTB0M2          TO WRK-MASCARA.
151209     DISPLAY '* QTDE INSERIDOS ADESAO CELULAR      = '
151209                                                 WRK-MASCARA ' *'.
151209
151209     MOVE WRK-ACU-ATLZ-GFCTB0M2          TO WRK-MASCARA.
151209     DISPLAY '* QTDE BAIXADOS ADESAO CELULAR       = '
151209                                                 WRK-MASCARA ' *'.
151209
151209     MOVE WRK-ACU-SEM-GFCTB0M2           TO WRK-MASCARA.
151209     DISPLAY '* QTDE ADESOES SEM CELULAR           = '
151209                                                 WRK-MASCARA ' *'.
151209
           MOVE WRK-ACU-ADES-ENCONTR-PAS       TO WRK-MASCARA.
           DISPLAY '* QTDE ADESOES ENCONTRADAS PASSADAS  = '
                                                       WRK-MASCARA ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '**************************************************'.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *            ROTINA DE TRATAMENTO DE CHECKPOINT/RESTART          *
      *----------------------------------------------------------------*
       9300-ROTINA-CHECKPONT-RESTART   SECTION.
      *----------------------------------------------------------------*

           CALL WRK-CKRS0100           USING CKRS01-INTERFACE.

           IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE CK01-TEXTO-MENSAGEM(01:75)
                                       TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9400-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           CLOSE CANADEEN
                 CANADESA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0900-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       9400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2109'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


