      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1676.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *              C P M  B R A X I S  -  S I S T E M A S            *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1676                                     *
      *    PROGRAMADORA:  SIMONI FAVRETTO         - CPM BRAXIS - P BCO *
      *    ANALISTA CPM:  KHARUZO INOCENCIO LEITE - CPM BRAXIS - P BCO *
      *    ANALISTA....:  VINICIUS C. MADUREIRA   - PROCKWORK  - GP 50 *
      *    DATA........:  27/09/2007                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  EXCLUIR DO PROCESSO DE COBRANCA OS REGISTROS *
      *      QUE POSSUAM TARIFA PROMOCIONAL.                           *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                  ARQENT                      I#GFCTVF          *
      *                  ISENTO92                    I#GFCTVA          *
      *                  ISENTO41                    I#GFCTV4          *
      *                  CONTINUA                    I#GFCTVF          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                          INCLUDE/BOOK   *
      *                    DB2PRD.PARM_SERVC_TARIF        GFCTB0A2     *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA UTILIZADA PELA BRAD7100.                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERRO.                              *
      *    BRAD7600 - OBTER DATA E HORA.                               *
      *    BRAD1050 - CONEXAO COM DB2.                                 *
      *================================================================*
      *
0208  *================================================================*
0208  *                     A L T E R A C A O                          *
0208  *----------------------------------------------------------------*
0208  *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
0208  *    DATA........:   12/02/2008                                  *
0208  *    OBJETIVO....:   SUBSTITUICAO DA INC I#GFCTVF PELA I#GFCTVA  *
0208  *================================================================*
RU0125*================================================================*
RU0125*                        A L T E R A C A O                       *
RU0125*----------------------------------------------------------------*
RU0125* ANALISTA       :  MARCIO ALEXANDRO RUI                         *
RU0125* DATA           :  JAN/2025                                     *
RU0125* OBJETIVO       :  INCLUSAO DE CPTCAO-TABELA                    *
RU0125*================================================================*
      *
      *
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQENT ASSIGN        TO UT-S-ARQENT
           FILE STATUS                 IS WRK-FS-ARQENT.

           SELECT ISENTO92 ASSIGN      TO UT-S-ISENTO92
           FILE STATUS                 IS WRK-FS-ISENTO92.

           SELECT ISENTO41 ASSIGN      TO UT-S-ISENTO41
           FILE STATUS                 IS WRK-FS-ISENTO41.

           SELECT CONTINUA ASSIGN      TO UT-S-CONTINUA
           FILE STATUS                 IS WRK-FS-CONTINUA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE ENTRADA  -   ARQENT                      *
      *            ORG. SEQUENCIAL     -   LRECL   =   134             *
      *----------------------------------------------------------------*

       FD  ARQENT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

0208***01 FD-ARQENT                  PIC  X(134).
0208   01 FD-ARQENT                  PIC  X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   ISENTO92                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   250             *
      *----------------------------------------------------------------*

       FD  ISENTO92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-ISENTO92                  PIC  X(250).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   ISENTO41                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   150             *
      *----------------------------------------------------------------*

       FD  ISENTO41
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       01 FD-ISENTO41                  PIC  X(150).

      *----------------------------------------------------------------*
      *   OUTPUT:  ARQUIVO DE SAIDA    -   CONTINUA                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   134             *
      *----------------------------------------------------------------*

       FD  CONTINUA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

0208***01 FD-CONTINUA                  PIC  X(134).
0208   01 FD-CONTINUA                  PIC  X(250).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'INICIO DA WORKING SECTION STORAGE'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ARQENT            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ISENTO41          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ISENTO92          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-CONTINUA          PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA PARA INDEXADOR'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.Z99    VALUE SPACES.
       77  WRK-DATA-REGRA              PIC  X(010)         VALUE SPACES.

       01  WRK-CHAVE-ATU.
           05  WRK-CSERVC-TARIF-ATU    PIC  S9(005)        VALUE ZEROS.
           05  WRK-DOCOR-EVNTO-ATU     PIC   X(010)        VALUE SPACES.

       01  WRK-CHAVE-ANT.
           05  WRK-CSERVC-TARIF-ANT    PIC  S9(005)        VALUE ZEROS.
           05  WRK-DOCOR-EVNTO-ANT     PIC   X(010)        VALUE SPACES.

RU0125 01 WRK-CPTCAO-GFCTB041          PIC  X(010) VALUE SPACES.
RU0125 01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB041.
           05 WRK-CPTCAO-B041          PIC 9(002).
RU0125     05 FILLER                   PIC X(008).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'AREA PARA TESTES DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQENT               PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ISENTO92             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ISENTO41             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-CONTINUA             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD7100.
           05 FILLER                   PIC  X(009)         VALUE
              '*** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE ' ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA LAYOUT DE ARQUIVOS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVA'.
       COPY 'I#GFCTV4'.
0208***COPY 'I#GFCTVF'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA BRAD7600'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05 WRK-DATA-JULIANA         PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAMMDD          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAAAMMDD        PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSS          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSSMMMMMM    PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TIMESTAMP.
             10 WRK-ANO                PIC  9(004)         VALUE ZEROS.
             10 WRK-MES                PIC  9(002)         VALUE ZEROS.
             10 WRK-DIA                PIC  9(002)         VALUE ZEROS.
             10 WRK-HOR                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIN                PIC  9(002)         VALUE ZEROS.
             10 WRK-SEG                PIC  9(002)         VALUE ZEROS.
             10 WRK-MSE                PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD7100'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A2
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING SECTION STORAGE'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    PROCEDIMENTOS INICIAIS DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ARQENT       EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ABERTURA DOS ARQUIVOS E TESTE DE FILE-STATUS.               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT  ARQENT
                OUTPUT ISENTO92
                       ISENTO41
                       CONTINUA.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DOS ARQUIVOS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ARQENT.

           PERFORM 1120-TESTAR-FS-ISENTO92.

           PERFORM 1130-TESTAR-FS-ISENTO41.

           PERFORM 1140-TESTAR-FS-CONTINUA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO ARQENT.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQENT           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQENT           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQENT'           TO  WRK-NOME-ARQ
               MOVE  WRK-FS-ARQENT     TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ARQUIVO ISENTO92.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ISENTO92         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ISENTO92         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ISENTO92'         TO  WRK-NOME-ARQ
               MOVE  WRK-FS-ISENTO92   TO  WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO ISENTO41.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-ISENTO41         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ISENTO41         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ISENTO41'         TO WRK-NOME-ARQ
               MOVE  WRK-FS-ISENTO41   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTAR FILE-STATUS DO CONTINUA.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-CONTINUA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CONTINUA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'CONTINUA'         TO WRK-NOME-ARQ
               MOVE  WRK-FS-CONTINUA   TO WRK-FILE-STATUS
               MOVE  WRK-ERRO-BRAD7100 TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR SE ARQUIVO ARQENT VAZIO.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ARQENT.

           IF  WRK-FS-ARQENT           EQUAL '10'
               DISPLAY '***************** GFCT1676 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO ARQENT VAZIO            *'
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1676 *****************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO ARQENT.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ARQENT               SECTION.
      *----------------------------------------------------------------*

0802*******READ ARQENT               INTO REG-GFCTVF.
0802       READ ARQENT               INTO GFCTVA-GFCTB092.

           IF  WRK-FS-ARQENT         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ARQENT.

           MOVE GFCTVA-CSERVC-TARIF    TO WRK-CSERVC-TARIF-ATU.
           MOVE GFCTVA-DOCOR-EVNTO     TO WRK-DOCOR-EVNTO-ATU.

           ADD 1                       TO ACU-LIDOS-ARQENT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  WRK-CHAVE-ATU           NOT EQUAL WRK-CHAVE-ANT
               PERFORM 3100-BUSCA-INDIC-GFCTB0A2
           ELSE
               IF  CINDCD-TARIF-PROML  OF GFCTB0A2
                                       EQUAL 'S'
                   PERFORM 3120-GRAVAR-ISENTO92
                   PERFORM 3130-GRAVAR-ISENTO41
               ELSE
                   PERFORM 3110-GRAVAR-CONTINUA
               END-IF
           END-IF.

           MOVE WRK-CHAVE-ATU          TO WRK-CHAVE-ANT.

           PERFORM 2100-LER-ARQENT.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCAR INDICADOR DE TARIFA PROMOCIONAL NA TABELA GFCTB0A2.  *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-BUSCA-INDIC-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTVA-CSERVC-TARIF     TO CSERVC-TARIF OF GFCTB0A2.
           MOVE GFCTVA-DOCOR-EVNTO      TO WRK-DATA-REGRA.

           EXEC SQL
             SELECT
               CSERVC_TARIF,
               DINIC_VGCIA_TARIF,
               DFIM_VGCIA_TARIF,
               CINDCD_TARIF_PROML
             INTO
               :GFCTB0A2.CSERVC-TARIF,
               :GFCTB0A2.DINIC-VGCIA-TARIF,
               :GFCTB0A2.DFIM-VGCIA-TARIF,
               :GFCTB0A2.CINDCD-TARIF-PROML
             FROM DB2PRD.PARM_SERVC_TARIF
             WHERE CSERVC_TARIF        = :GFCTB0A2.CSERVC-TARIF
             AND   DINIC_VGCIA_TARIF  <= :WRK-DATA-REGRA
             AND   DFIM_VGCIA_TARIF   >= :WRK-DATA-REGRA
             ORDER  BY HIDTFD_MANUT_SERVC DESC
ATEND        FETCH FIRST 1 ROW ONLY
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'PARM_SERC_TARIF '    TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0010'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 3110-GRAVAR-CONTINUA
           ELSE
               IF  CINDCD-TARIF-PROML  OF GFCTB0A2
                                       EQUAL 'S'
                   PERFORM 3120-GRAVAR-ISENTO92
                   PERFORM 3130-GRAVAR-ISENTO41
               ELSE
                   PERFORM 3110-GRAVAR-CONTINUA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR REGISTROS NO ARQUIVO CONTINUA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3110-GRAVAR-CONTINUA            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-CONTINUA           FROM GFCTVA-GFCTB092.

           PERFORM 1140-TESTAR-FS-CONTINUA.

           ADD 1                       TO ACU-GRAVA-CONTINUA.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *     MOVER DADOS DO ARQUIVO ARQENT PARA ARQUIVO ISENTO92.       *
      ******************************************************************
      *----------------------------------------------------------------*
       3120-GRAVAR-ISENTO92            SECTION.
      *----------------------------------------------------------------*

0208***    MOVE GFCTVA-GFCTB092            OF GFCTVA-GFCTB092
0208***                                TO GFCTVA-GFCTB092.

           MOVE ZEROS                  TO GFCTVA-CTRATO-MOVTO
                                          GFCTVA-QTD-FQ-COMPON
                                          GFCTVA-QTD-EXC-DIA
                                          GFCTVA-QTD-EXC-ACU
                                          GFCTVA-AGENCIA-CREC
                                          GFCTVA-CONTA-CREC
                                          GFCTVA-PACOTE-EVENTO
                                          GFCTVA-CSGMTO-GSTAO-TARIF
                                          GFCTVA-CMUN-RURAL
                                          GFCTVA-BCO-POSTAL
                                          GFCTVA-CPAB
                                          GFCTVA-CPF-CNPJ
                                          GFCTVA-FILIAL
                                          GFCTVA-CSEC-COMP-PCT
                                          GFCTVA-DINI-PER-COMP
                                          GFCTVA-DFIM-PER-COMP
                                          GFCTVA-PERIODO-PCT
                                          GFCTVA-CPER-COBR-COMPO
                                          GFCTVA-VEXCED-CP-PCT
                                          GFCTVA-PEXCED-CP-PCT
                                          GFCTVA-QDIA-COBR-COMPO
                                          GFCTVA-VTARIF-LIQ-MOVTO.

           PERFORM VARYING IND-1       FROM 1 BY 1
               UNTIL IND-1             GREATER 7
               MOVE ZEROS              TO GFCTVA-AGPT-REG(IND-1)
           END-PERFORM.

           MOVE SPACES                 TO GFCTVA-FLAG-DATA
                                          GFCTVA-CSGL-UF
                                          GFCTVA-CONTROLE.

           WRITE FD-ISENTO92           FROM GFCTVA-GFCTB092.

           PERFORM 1120-TESTAR-FS-ISENTO92.

           ADD 1                       TO ACU-GRAVA-ISENTO92.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *     MOVER DADOS DO ARQUIVO ARQENT PARA ARQUIVO ISENTO92.       *
      ******************************************************************
      *----------------------------------------------------------------*
       3130-GRAVAR-ISENTO41            SECTION.
      *----------------------------------------------------------------*
0208  *-->> TODOS OS CAMPOS DA VF SUBSTITUIDOS POR VA

           MOVE SPACES                 TO GFCTV4-GFCTB041.

           INITIALIZE GFCTV4-GFCTB041.

           MOVE GFCTVA-DENVIO-MOVTO-TARIF  OF GFCTVA-GFCTB092
                                       TO GFCTV4-DENVIO-MOVTO-TARIF.
           MOVE GFCTVA-CROTNA-ORIGE-MOVTO  OF GFCTVA-GFCTB092
                                       TO GFCTV4-CROTNA-ORIGE-MOVTO.
           MOVE GFCTVA-CNRO-ARQ-MOVTO      OF GFCTVA-GFCTB092
                                       TO GFCTV4-CNRO-ARQ-MOVTO.
           MOVE GFCTVA-CSEQ-MOVTO          OF GFCTVA-GFCTB092
                                       TO GFCTV4-CSEQ-MOVTO.
RU0125     MOVE  GFCTV4-DENVIO-MOVTO-TARIF
RU0125                                 TO WRK-CPTCAO-GFCTB041.
RU0125     MOVE  WRK-CPTCAO-B041       TO GFCTV4-CPTCAO.
           MOVE 4                      TO GFCTV4-CIDTFD-TPO-DESC.
           MOVE 3                      TO GFCTV4-CIDTFD-MOTVO-DESC.
           MOVE 0                      TO GFCTV4-CIDTFD-DESC.
           MOVE 3                      TO GFCTV4-CPARM-DESC.
           MOVE GFCTVA-QEVNTO-AGRUP        OF GFCTVA-GFCTB092
                                       TO GFCTV4-QEVNTO-DESC.
           MOVE GFCTVA-VTARIF-LIQ-MOVTO    OF GFCTVA-GFCTB092
                                       TO GFCTV4-VCONCS-DESC.
           MOVE SPACES                 TO GFCTV4-CCONTR-FLEXZ.
           MOVE GFCTVA-CBCO-DSTNO-MOVTO    OF GFCTVA-GFCTB092
                                       TO GFCTV4-CBCO-DSTNO-MOVTO.
           MOVE GFCTVA-CAG-DSTNO-MOVTO     OF GFCTVA-GFCTB092
                                       TO GFCTV4-CAG-DSTNO-MOVTO.
           MOVE GFCTVA-CCTA-DSTNO-MOVTO    OF GFCTVA-GFCTB092
                                       TO GFCTV4-CCTA-DSTNO-MOVTO.
           MOVE GFCTVA-CPAB-DSTNO-MOVTO    OF GFCTVA-GFCTB092
                                       TO GFCTV4-CPAB-DSTNO-MOVTO.
           MOVE GFCTVA-CSERVC-TARIF        OF GFCTVA-GFCTB092
                                       TO GFCTV4-CSERVC-TARIF.
           MOVE GFCTVA-DOCOR-EVNTO         OF GFCTVA-GFCTB092
                                       TO GFCTV4-DOCOR-EVNTO.
           MOVE GFCTVA-HPREST-SERVC-MOVTO  OF GFCTVA-GFCTB092
                                       TO GFCTV4-HPREST-MOVTO.
           MOVE WRK-TIMESTAMP          TO GFCTV4-IDTFD-TIMESTAMP.

           WRITE FD-ISENTO41           FROM GFCTV4-GFCTB041.

           PERFORM 1130-TESTAR-FS-ISENTO41.

           ADD 1                       TO ACU-GRAVA-ISENTO41.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROGRAMA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ARQENT
                 ISENTO92
                 ISENTO41
                 CONTINUA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMISSAO DE TOTAIS DE LIDOS E GRAVADOS.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT1676 ********************'.
           DISPLAY '*                                                *'.
           MOVE ACU-LIDOS-ARQENT     TO WRK-MASCARA.
           DISPLAY '* REGISTROS LIDOS EM ARQENT     :    ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVA-ISENTO92     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS NO ISENTO92:    ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVA-ISENTO41     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS NO ISENTO41:    ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           MOVE ACU-GRAVA-CONTINUA     TO WRK-MASCARA.
           DISPLAY '* REGISTROS GRAVADOS NO CONTINUA:    ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1676 ********************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO PARA CANCELAMENTO DO PROGRAMA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1676'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

