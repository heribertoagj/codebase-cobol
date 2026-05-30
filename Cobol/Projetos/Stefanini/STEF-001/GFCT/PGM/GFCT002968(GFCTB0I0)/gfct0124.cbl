      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0124.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT0124                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: MARCUS VINICIUS                          *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: MAR/2010                                 *
      *                                                                *
      *    PROJETO..........: ADESAO DE CESTAS CELULAR                 *
      *                                                                *
      *    OBJETIVO.........: GERAR CONSULTA DE NRO CELULAR VIGENTE A  *
      *                       UMA ADESAO.                              *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.V01UF                      DCITV002              *
BI0412*        DB2PRD.PRMSS_ADSAO_PCOTE          GFCTB0C6              *
      *        DB2PRD.PRMSS_GRP_DEPDC            GFCTB0C7              *
      *        DB2PRD.PRMSS_GRP_EMPR             GFCTB0C8              *
      *        DB2PRD.PRMSS_GRP_MUN              GFCTB0C9              *
      *        DB2PRD.PRMSS_GRP_PAB              GFCTB0D0              *
      *        DB2PRD.PRMSS_GRP_PSTAL            GFCTB0D1              *
      *        DB2PRD.PRMSS_GRP_SGMTO            GFCTB0D2              *
      *        DB2PRD.PRMSS_GRP_UF               GFCTB0D3              *
      *        DB2PRD.TPRMSS_GRP_CLI             GFCTB0I0              *
      *        DB2PRD.TTPO_SGMTO_GSTAO           GFCTB0H5              *
      *        DB2PRD.SERVC_TARIF_PRINC          GFCTB0D8              *
      *        DB2PRD.TPO_AGPTO_CTA              GFCTB0F3              *
      *                                                                *
      *    BOOK'S...........:                                          *
      *        GFCTWAMW - ENT                                          *
      *        GFCTWAMX - SAI                                          *
      *        I#GFCTFZ - ENT - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG1 - SAI - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG2 - ENT - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCTG3 - SAI - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCTG0 - ENT - OBTER NOME DO CLIENTE.                 *
      *        I#GFCTHA - SAI - OBTER NOME DO CLIENTE.                 *
      *        I#GFCT0M - AREA DE COMUNICACAO - ERROS.                 *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.        *
      *        GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                *
      *        GFCT5109 - OBTER DESCRICAO PAB.                         *
      *        GFCT5006 - OBTER DESCRICAO DEPDC.                       *
      *        GFCT5054 - OBTER DADOS DO CLIENTE.                      *
      *        GFCT5090 - OBTER DESCRICAO POSTO.                       *
      *        RURC9020 - OBTER DESCRICAO MUNICIPIO.                   *
      *        DCITV002 - OBTER DESCRICAO UF.                          *
      *        SARH8000 - PERMITIR AOS SISTEMAS LEGADOS ACESSO AS IN-  *
      *                   FORMACOES DE FUNCIONARIOS NO REPOSITORIO DE  *
      *                   DADOS SAP                                    *
      *                                                                *
      *================================================================*
BI0810*                    A L T E R A C A O                           *
BI0810*----------------------------------------------------------------*
BI0810*                                                                *
BI0810*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0810*    DATA         : AGO/2010                                     *
BI0810*                                                                *
BI0810*    OBJETIVO     :                                              *
BI0810*    - SELECIONA O CAMPO (TIPO DE PESSOA) DA TABELA GFCTB0C6     *
BI0810*                                                                *
BI0810*================================================================*
BI0511*                 U L T I M A   A L T E R A C A O                *
BI0511*----------------------------------------------------------------*
BI0511*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0511       *
BI0511*----------------------------------------------------------------*
BI0511*                                                                *
BI0511*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0511*    DATA........:  05 / 2011                                    *
BI0511*    OBJETIVO....:  PROJETO CARTAO BONUS CELULAR - FASE II       *
BI0511*                                                                *
BI0412*----------------------------------------------------------------*
BI0412*                 U L T I M A   A L T E R A C A O                *
BI0412*----------------------------------------------------------------*
BI0412*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0412       *
BI0412*----------------------------------------------------------------*
BI0412*                                                                *
BI0412*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0412*    DATA........:  04 / 2012                                    *
BI0412*    OBJETIVO....:  SETAR FLAG PARA PERMITIR VIZUALIZACAO        *
BI0412*                   DE ADESAO COMPULSORIA, A ADESAO COMPULSORIA  *
BI0412*                   ASSINALADA NO ONLINE SERA INSERIDA NA        *
BI0412*                   BASE GFCTB009 DE ADESAO INDIVIDUAL.          *
BI0412*                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(001) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

       01  WRK-MSG01.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-NOME-MOD             PIC  X(008)         VALUE SPACES.

       01  WRK-TAM-01-COM-S            PIC -9(001)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-01-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-01-SEM-S        PIC  9(001).

       01  WRK-TAM-02-COM-S            PIC -9(002)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-02-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-02-SEM-S        PIC  9(002).

       01  WRK-TAM-03-COM-S            PIC -9(003)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-03-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-03-SEM-S        PIC  9(003).

       01  WRK-TAM-05-COM-S            PIC -9(005)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-05-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-05-SEM-S        PIC  9(005).

       01  WRK-TAM-07-COM-S            PIC -9(007)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-07-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-07-SEM-S        PIC  9(007).

       01  WRK-TAM-09-COM-S            PIC -9(009)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-09-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-09-SEM-S        PIC  9(009).

       01  WRK-S9-9                    PIC -9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-9.
           05  FILLER                  PIC  X(001).
           05  WRK-9-9                 PIC  9(009).
           05  FILLER                  REDEFINES WRK-9-9.
               07  FILLER              PIC  X(002).
               07  WRK-X-7             PIC  X(007).

       01  FILLER.
           03  WRK-TIMESTAMP.
               05  WRK-ANO             PIC  9(04)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '-'.
               05  WRK-MES             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '-'.
               05  WRK-DIA             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '-'.
               05  WRK-HORA            PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '.'.
               05  WRK-MIN             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '.'.
               05  WRK-SEG             PIC  9(02)       VALUE ZEROS.
               05  FILLER              PIC  X(01)       VALUE '.'.
               05  WRK-NAN             PIC  9(06)       VALUE ZEROS.

       01  WRK-TIME-X.
           05  WRK-HORA-X          PIC  9(02)       VALUE ZEROS.
           05  FILLER              PIC  X(01)       VALUE '.'.
           05  WRK-MINU-X          PIC  9(02)       VALUE ZEROS.

       01  WRK-DESC-FUNCIONARIO        PIC  X(040)        VALUE SPACES.

       01  WRK-COD-FUNC                PIC 9(009)        VALUE ZEROS.
       01  WRK-COD-FUNC-R              REDEFINES WRK-COD-FUNC.
           05  FILLER                  PIC  X(002).
           05  WRK-COD-FUNC-X7          PIC  X(007).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MODULO DCIT8000 *'.
      *---------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           03  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           03  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

       01  WRK-POOL7100                 PIC X(107)         VALUE SPACES.
       01  WRK-SQLCA                    PIC X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MODULO SARH8000 ***'.
      *----------------------------------------------------------------*

       01  WRK-ENTRADA.
           03  WRK-PESQUISA-ENT        PIC  9(009)         VALUE ZEROS.
           03  WRK-TAMANHO-ENT         PIC  9(004)         VALUE ZEROS.
           03  WRK-SOLICITA-ENT.
               05  FILLER              OCCURS 255 TIMES.
                   07  WRK-CAMPO-ENT   PIC  9(003)         VALUE ZEROS.

       01  WRK-SAIDA.
           03  WRK-COD-RETORNO-SAI     PIC  9(002)         VALUE ZEROS.
           03  WRK-TAMANHO-SAI         PIC  9(004)         VALUE ZEROS.
           03  WRK-REGISTRO-SAI.
               05  FILLER              OCCURS 0 TO 1000 TIMES
                   DEPENDING ON WRK-TAMANHO-SAI.
                   07  WRK-CONTADOR-SAI PIC  X(001)        VALUE SPACES.

       01  WRK-ERRO-AREA               PIC  X(107)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#RURC88'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5054'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG0'.

       COPY 'I#GFCTHA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *---------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C7
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0I0
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0C9
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D3
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0F3
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA AREA DE WORKING *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAMW'.

       COPY 'GFCTWAMX'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTWAMW-ENTRADA
                                                      GFCTWAMX-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *ROTINA INICIAL DO PROGRAMA                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *MONTA AREA DE SAIDA, VERIFICA DISPONIBILIDADE DO SISTEMA E CON- *
      *SISTE DADOS.                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTWAMX-SAIDA.

           INITIALIZE GFCTWAMX-SAIDA
                      IND-1
                      GFCT0M-AREA-ERROS.

           PERFORM 1100-MONTAR-AREA-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

           PERFORM 1300-VERIFICAR-DISP-SISTEMA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *----------------------------------------------------------------*
       1100-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTWAMX-SAIDA.

           MOVE LENGTH OF GFCTWAMX-SAIDA TO WAMX-LL.
           MOVE ZEROS                  TO WAMX-ZZ.
           MOVE WAMW-TRANSACAO         TO WAMX-TRANSACAO.
           MOVE WAMW-FUNCAO            TO WAMX-FUNCAO.
           MOVE WAMW-FUNC-BDSCO        TO WAMX-FUNC-BDSCO.

           MOVE WAMW-COD-TARIFA        TO WAMX-COD-TARIFA
           MOVE WAMW-COD-AGPTO         TO WAMX-COD-AGPTO
           MOVE WAMW-SEQUENCIA         TO WAMX-SEQUENCIA
           MOVE WAMW-DATA-INICIO       TO WAMX-DATA-INICIO

           MOVE 'S'                    TO WAMX-FIM.

           MOVE ZEROS                  TO WAMX-ERRO
                                          WAMX-COD-SQL-ERRO
                                          WAMX-COD-MSG-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.
           MOVE  GFCTG3-DESC-MSG       TO WAMX-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *OBTEM DESCRICAO DA MENSAGEM                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAMW-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAMW-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAMW-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO          USING GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS OR
               GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO WAMX-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0124'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO WAMX-ERRO
               MOVE 'S'                    TO WAMX-FIM
               MOVE GFCTG3-COD-SQL-ERRO    TO WAMX-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO WAMX-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA CONSISTIR DADOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  WAMW-TRANSACAO          EQUAL LOW-VALUES        OR
               WAMW-TRANSACAO          EQUAL SPACES            OR
               WAMW-FUNCAO             EQUAL LOW-VALUES        OR
               WAMW-FUNCAO             EQUAL SPACES            OR
               WAMW-FUNC-BDSCO         EQUAL LOW-VALUES        OR
               WAMW-FUNC-BDSCO         EQUAL SPACES            OR
               WAMW-COD-TARIFA         NOT NUMERIC             OR
               WAMW-COD-TARIFA         EQUAL ZEROS             OR
               WAMW-COD-AGPTO          NOT NUMERIC             OR
               WAMW-COD-AGPTO          EQUAL ZEROS             OR
               WAMW-SEQUENCIA          NOT NUMERIC             OR
               WAMW-SEQUENCIA          EQUAL ZEROS             OR
               WAMW-DATA-INICIO        EQUAL LOW-VALUES        OR
               WAMW-DATA-INICIO        EQUAL SPACES
               MOVE 1                  TO WAMX-ERRO
                                          WAMX-COD-MSG-ERRO
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO WAMX-DESC-MSG-ERRO
               MOVE 'S'                TO WAMX-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA MOVER ERRO SQL                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1211-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO WAMX-ERRO.
           MOVE 'GFCT0124'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
                                          WRK-TAM-09-COM-S
           MOVE  WRK-TAM-09-SEM-S(7:3) TO WAMX-COD-SQL-ERRO.
           MOVE SPACES                 TO WAMX-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO WAMX-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTG1-SAIDA.

           MOVE 'GFCT5522'             TO WRK-MODULO.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAMW-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAMW-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE ZEROS                  TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.

           CALL WRK-MODULO          USING GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAMX-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0124'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO(1:75)
                                               TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO    TO WAMX-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT0124'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO                TO WAMX-ERRO
               MOVE GFCTG1-COD-MSG-ERRO        TO WAMX-COD-MSG-ERRO
               MOVE 'S'                        TO WAMX-FIM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO WAMX-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0124'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

      *--> OBTER DESCRICAO DA TARIFA

           PERFORM 2960-ACESSAR-GFCTB0D8.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8(1:40)
                                       TO WAMX-DESCR-TARIFA.

      *--> OBTER DESCRICAO DA TARIFA

           PERFORM 2970-ACESSAR-GFCTB0F3.

           MOVE RAGPTO-CTA             OF GFCTB0F3
                                       TO WAMX-DESCR-AGPTO.

      *--> OBTER CAMPOS DA SAIDA

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0C6.

           PERFORM 2900-ACESSAR-GFCTB0C6.

           MOVE CINDCD-ADSAO-INDVD OF GFCTB0C6
                                       TO WAMX-TP-ADESAO

           IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'C'
               IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
                   MOVE  1             TO WAMX-SOLICITA-EXCECAO
               ELSE
                   MOVE  0             TO WAMX-SOLICITA-EXCECAO
               END-IF
BI0810         MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0810                                 TO WAMX-CPSSOA-TARIFA
BI0412         MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO WAMX-VISUAL-COMPULS
BI0412     END-IF.

BI0412     IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'I'
               MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
                                       TO WRK-TAM-01-COM-S
BI0412         MOVE WRK-TAM-01-SEM-S   TO WAMX-PRMSS-INDVD
           END-IF.

BI0412     IF  CINDCD-ADSAO-INDVD OF GFCTB0C6 EQUAL 'A'
BI0412         MOVE CPSSOA-SERVC-TARIF OF GFCTB0C6
BI0412                                 TO WAMX-CPSSOA-TARIFA
BI0412         MOVE CIDTFD-PRMSS-INDVD OF GFCTB0C6
BI0412                                 TO WRK-TAM-01-COM-S
BI0412         MOVE WRK-TAM-01-SEM-S   TO WAMX-PRMSS-INDVD
BI0412         MOVE CINDCD-VSLAO-COMP  OF GFCTB0C6
BI0412                                 TO WAMX-VISUAL-COMPULS
BI0412         IF  CINDCD-EXCEC-ADSAO OF GFCTB0C6  EQUAL 'S'
BI0412             MOVE  1             TO WAMX-SOLICITA-EXCECAO
BI0412         ELSE
BI0412             MOVE  0             TO WAMX-SOLICITA-EXCECAO
BI0412         END-IF
BI0412     END-IF.

           MOVE DFIM-PRMSS-PCOTE       OF GFCTB0C6
                                       TO WAMX-DATA-FIM

BI0511     MOVE ZEROS                  TO WAMX-GERA-CRED-CEL

           PERFORM 2950-VALORIZAR-FUNC.


           EVALUATE WAMW-COD-AGPTO
               WHEN 15
                   PERFORM 2100-CONSULTAR-PAB

               WHEN 14
                   PERFORM 2200-CONSULTAR-AGENCIA

               WHEN 16
                   PERFORM 2300-CONSULTAR-EMPRESA

               WHEN 17
                   PERFORM 2400-CONS-POSTAL-EXPRESSO

               WHEN 20
                   PERFORM 2400-CONS-POSTAL-EXPRESSO

               WHEN 4
                   PERFORM 2500-CONSULTAR-CLIENTE

               WHEN 9
                   PERFORM 2600-CONSULTAR-SEGMENTO

               WHEN 18
                   PERFORM 2700-CONSULTAR-MUNICIPIO

               WHEN 19
                   PERFORM 2800-CONSULTAR-UF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-CONSULTAR-PAB              SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-ACESSAR-GFCTB0D0.

           MOVE CDEPDC       OF GFCTB0D0   TO WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S           TO WAMX-AGENCIA

           MOVE CPOSTO-SERVC OF GFCTB0D0   TO WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S           TO WAMX-POSTO

           IF  CINDCD-AGPTO-TOT OF GFCTB0C6 EQUAL 'S'
               MOVE 'TODOS'                TO WAMX-DESCR-AGPTO
           ELSE
               PERFORM 2120-OBTER-DESC-PAB
               MOVE GFCTGC-DESC-POSTO(1)   TO WAMX-DESCR-AGPTO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0D0                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ACESSAR-GFCTB0D0              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0D0.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.

           EXEC SQL
               SELECT
                   CDEPDC ,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0D0.CDEPDC ,
                   :GFCTB0D0.CPOSTO-SERVC
               FROM
                   DB2PRD.PRMSS_GRP_PAB
               WHERE
                   CSERVC_TARIF        = :GFCTB0D0.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0D0.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0D0.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0D0.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D0'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PAB   ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE  0006              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                         TO GFCTGB-ENTRADA
                                                  GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.

           INITIALIZE                          GFCTGB-ENTRADA
                                               GFCTGC-SAIDA
                                               GFCT0M-AREA-ERROS.

           MOVE 100                            TO GFCTGB-LL.
           MOVE ZEROS                          TO GFCTGB-ZZ.
           MOVE WAMW-TRANSACAO                 TO GFCTGB-TRANSACAO.
           MOVE WAMW-FUNCAO                    TO GFCTGB-FUNCAO.
           MOVE ZEROS                          TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE WAMW-FUNC-BDSCO                TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.

           MOVE CDEPDC        OF GFCTB0D0      TO WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S               TO GFCTGB-COD-DEPDC.

           MOVE CPOSTO-SERVC  OF GFCTB0D0      TO WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S               TO GFCTGB-COD-POSTO.

           MOVE 'GFCT5109'                     TO WRK-MODULO.

           CALL WRK-MODULO                  USING GFCTGB-ENTRADA
                                                  GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO WAMX-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0124'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO WAMX-FIM
               MOVE GFCTGC-ERRO                TO WAMX-ERRO
               MOVE GFCTGC-COD-SQL-ERRO        TO WAMX-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO        TO WAMX-COD-MSG-ERRO
               IF  GFCTGC-ERRO                 EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO   TO WAMX-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109-'          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE           INTO WAMX-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-CONSULTAR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ACESSAR-GFCTB0C7.

           MOVE CDEPDC OF GFCTB0C7     TO WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S       TO WAMX-AGENCIA.

           IF  CINDCD-AGPTO-TOT OF GFCTB0C6 EQUAL 'S'
               MOVE 'TODAS'            TO WAMX-DESC-AGE
           ELSE
               PERFORM 2220-OBTER-DESC-DEPDC
               MOVE GFCT1X-DESC-DEPDC(1)(1:40)
                                       TO WAMX-DESC-AGE
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0C7                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ACESSAR-GFCTB0C7              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0C7.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.

           EXEC SQL
               SELECT
                   CDEPDC
               INTO
                   :GFCTB0C7.CDEPDC
               FROM
                   DB2PRD.PRMSS_GRP_DEPDC
               WHERE
                   CSERVC_TARIF        = :GFCTB0C7.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C7.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C7.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0C7.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C7'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_DEPDC ' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0006              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-OBTER-DESC-DEPDC           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                     GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE WAMW-TRANSACAO         TO GFCT1V-TRANSACAO.
           MOVE WAMW-FUNCAO            TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE WAMW-FUNC-BDSCO        TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.

           MOVE CDEPDC OF GFCTB0C7     TO WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S       TO GFCT1V-COD-DEPDC(1).

           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO WAMX-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0124'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO WAMX-FIM
               MOVE GFCT1X-ERRO                TO WAMX-ERRO
               MOVE GFCT1X-COD-SQL-ERRO        TO WAMX-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO        TO WAMX-COD-MSG-ERRO
               IF  GFCT1X-ERRO                 EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO   TO WAMX-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'          GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE           INTO WAMX-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-CONSULTAR-EMPRESA          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2310-ACESSAR-GFCTB0C8.

           MOVE CJUNC-DEPDC-DEB        OF GFCTB0C8
                                       TO WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S       TO WAMX-AGENCIA

           MOVE CNRO-CTA-DEB           OF GFCTB0C8
                                       TO WRK-TAM-07-COM-S
           MOVE WRK-TAM-07-SEM-S       TO WAMX-CONTA

           IF  CINDCD-AGPTO-TOT OF GFCTB0C6 EQUAL 'S'
               MOVE 'TODAS'            TO WAMX-DESC-CLIE
           ELSE
               PERFORM 2320-OBTER-DESC-CLIE
               MOVE GFCTHA-NOME-CLIENTE(1:40)
                                       TO WAMX-DESC-CLIE
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0C8                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ACESSAR-GFCTB0C8              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0C8.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0C8.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0C8.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0C8.

           EXEC SQL
               SELECT
                   CJUNC_DEPDC_DEB,
                   CNRO_CTA_DEB
               INTO
                   :GFCTB0C8.CJUNC-DEPDC-DEB,
                   :GFCTB0C8.CNRO-CTA-DEB
               FROM
                   DB2PRD.PRMSS_GRP_EMPR
               WHERE
                   CSERVC_TARIF        = :GFCTB0C8.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C8.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C8.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0C8.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C8'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_EMPR  ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0005              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER NOME DO CLIENTE                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-OBTER-DESC-CLIE            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG0-ENTRADA.
           MOVE 100                    TO GFCTG0-LL.
           MOVE ZEROS                  TO GFCTG0-ZZ.
           MOVE WAMW-TRANSACAO         TO GFCTG0-TRANSACAO.
           MOVE WAMW-FUNCAO            TO GFCTG0-FUNCAO.
           MOVE WAMW-FUNC-BDSCO        TO GFCTG0-FUNC-BDSCO.

           MOVE CJUNC-DEPDC-DEB        OF GFCTB0C8
                                       TO WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S       TO GFCTG0-COD-DEPDC.

           MOVE CNRO-CTA-DEB           OF GFCTB0C8
                                       TO WRK-TAM-07-COM-S.
           MOVE WRK-TAM-07-SEM-S       TO GFCTG0-CCTA-CLI.

           MOVE SPACES                 TO GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE  GFCTHA-SAIDA
                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5054'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG0-ENTRADA
                                             GFCTHA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO WAMX-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0124'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO WAMX-FIM
               MOVE GFCTHA-ERRO                TO WAMX-ERRO
               MOVE GFCTHA-COD-SQL-ERRO        TO WAMX-COD-SQL-ERRO
               MOVE GFCTHA-COD-MSG-ERRO        TO WAMX-COD-MSG-ERRO
               IF  GFCTHA-ERRO                 EQUAL 1
                   MOVE GFCTHA-DESC-MSG-ERRO   TO WAMX-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006-'          GFCTHA-DESC-MSG-ERRO
                   DELIMITED BY SIZE           INTO WAMX-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-CONS-POSTAL-EXPRESSO       SECTION.
      *----------------------------------------------------------------*

           PERFORM 2410-ACESSAR-GFCTB0D1.

           MOVE CDEPDC                 OF GFCTB0D1
                                       TO WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S       TO WAMX-AGENCIA

           MOVE CPOSTO-SERVC           OF GFCTB0D1
                                       TO WRK-TAM-05-COM-S
           MOVE WRK-TAM-05-SEM-S       TO WAMX-POSTO

           IF  CINDCD-AGPTO-TOT OF GFCTB0C6 EQUAL 'S'
               MOVE 'TODAS'            TO WAMX-DESC-POSTO
           ELSE
               PERFORM 2420-OBTER-DESC-POSTO
               MOVE GFCTGC-DESC-POSTO(1)
                                       TO WAMX-DESC-POSTO
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0D1                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-ACESSAR-GFCTB0D1              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0D1.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0D1.

           EXEC SQL
               SELECT
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0D1.CDEPDC,
                   :GFCTB0D1.CPOSTO-SERVC
               FROM
                   DB2PRD.PRMSS_GRP_PSTAL
               WHERE
                   CSERVC_TARIF        = :GFCTB0D1.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0D1.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0D1.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0D1.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D1'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0005              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-OBTER-DESC-POSTO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTGB-ENTRADA
                                              GFCTGC-SAIDA
                                              GFCT0M-AREA-ERROS.

           INITIALIZE                         GFCTGB-ENTRADA
                                              GFCTGC-SAIDA
                                              GFCT0M-AREA-ERROS.

           MOVE 100                        TO GFCTGB-LL.
           MOVE ZEROS                      TO GFCTGB-ZZ.
           MOVE WAMW-TRANSACAO             TO GFCTGB-TRANSACAO.
           MOVE WAMW-FUNCAO                TO GFCTGB-FUNCAO.
           MOVE ZEROS                      TO GFCTGB-QTDE-OCOR
                                              GFCTGB-QTDE-TOT-REG.
           MOVE WAMW-FUNC-BDSCO            TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTGB-FIM.

           MOVE CDEPDC        OF GFCTB0D1  TO WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S           TO GFCTGB-COD-DEPDC.

           MOVE CPOSTO-SERVC  OF GFCTB0D1  TO WRK-TAM-05-COM-S.
           MOVE WRK-TAM-05-SEM-S           TO GFCTGB-COD-POSTO.

           MOVE 'GFCT5090'                 TO WRK-MODULO.

           CALL WRK-MODULO              USING GFCTGB-ENTRADA
                                              GFCTGC-SAIDA
                                              GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO WAMX-ERRO
               MOVE WRK-MODULO             TO WRK-NOME-MOD
                                              GFCT0M-PROGRAMA
               MOVE WRK-MSG01              TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0124'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               MOVE 'S'                        TO WAMX-FIM
               MOVE GFCTGC-ERRO                TO WAMX-ERRO
               MOVE GFCTGC-COD-SQL-ERRO        TO WAMX-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO        TO WAMX-COD-MSG-ERRO

               IF  GFCTGC-ERRO                 EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO   TO WAMX-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090-'          GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE           INTO WAMX-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2510-ACESSAR-GFCTB0I0.

ST25X6*    MOVE CCGC-CPF               OF GFCTB0I0
ST25X6*                                TO WRK-TAM-09-COM-S
ST25X6     MOVE CCGC-CPF-ST            OF GFCTB0I0
ST25X6                                 TO  WAMX-CPF-CNPJ

ST25X6*    MOVE CFLIAL-CGC             OF GFCTB0I0
ST25X6*                                TO WRK-TAM-05-COM-S
ST25X6     MOVE CFLIAL-CGC-ST          OF GFCTB0I0
                                       TO WAMX-FILIAL

ST25X6*    MOVE CCTRL-CPF-CGC          OF GFCTB0I0
ST25X6*                                TO WRK-TAM-02-COM-S
ST25X6     MOVE CCTRL-CPF-CGC-ST       OF GFCTB0I0
ST25X6                                 TO WRK-TAM-02-COM-S
           MOVE WRK-TAM-02-SEM-S       TO WAMX-CONTROLE.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0I0                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-ACESSAR-GFCTB0I0              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0I0.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0I0.

           EXEC SQL
               SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
ST25X6             CCGC_CPF_ST,
ST25X6             CFLIAL_CGC_ST,
ST25X6             CCTRL_CPF_CGC_ST

               INTO
                   :GFCTB0I0.CCGC-CPF,
                   :GFCTB0I0.CFLIAL-CGC,
                   :GFCTB0I0.CCTRL-CPF-CGC,
ST25X6             :GFCTB0I0.CCGC-CPF-ST,
ST25X6             :GFCTB0I0.CFLIAL-CGC-ST,
ST25X6             :GFCTB0I0.CCTRL-CPF-CGC-ST
               FROM
                   DB2PRD.TPRMSS_GRP_CLI
               WHERE
                   CSERVC_TARIF        = :GFCTB0I0.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0I0.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0I0.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0I0.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0I0'      TO WRK-NOME-TAB
               MOVE 'TPRMSS_GRP_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0005              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-CONSULTAR-SEGMENTO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-ACESSAR-GFCTB0D2.

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB0D2
                                       TO WRK-TAM-03-COM-S
           MOVE WRK-TAM-03-SEM-S       TO WAMX-SEGMENTO

           PERFORM 2620-OBTER-DESC-SEGMENTO

           MOVE ISGMTO-GSTAO-TARIF     OF GFCTB0H5
                                       TO WAMX-DESC-SEGTO.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0D2                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-ACESSAR-GFCTB0D2              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0D2.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0D2.

           EXEC SQL
               SELECT
                   CSGMTO_GSTAO_TARIF
               INTO
                   :GFCTB0D2.CSGMTO-GSTAO-TARIF
               FROM
                   DB2PRD.PRMSS_GRP_SGMTO
               WHERE
                   CSERVC_TARIF        = :GFCTB0D2.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0D2.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0D2.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0D2.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D2'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0005              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2620-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF OF GFCTB0D2 TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB0H5.

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO  :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM  DB2PRD.TTPO_SGMTO_GSTAO
             WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0H5'      TO WRK-NOME-TAB
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0012              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-CONSULTAR-MUNICIPIO        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-ACESSAR-GFCTB0C9.

           MOVE CMUN-IBGE              OF GFCTB0C9
                                       TO WRK-TAM-07-COM-S
           MOVE WRK-TAM-07-SEM-S       TO WAMX-MUNICIPIO

           PERFORM 2720-OBTER-DESC-MUNICIPIO

           MOVE NOME-MUNIC-RURC88      TO WAMX-DESC-MUNIC.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0C9                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ACESSAR-GFCTB0C9              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0C9.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.

           EXEC SQL
               SELECT
                   CMUN_IBGE
               INTO
                   :GFCTB0C9.CMUN-IBGE
               FROM
                   DB2PRD.PRMSS_GRP_MUN
               WHERE
                   CSERVC_TARIF        = :GFCTB0C9.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0C9.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0C9.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0C9.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C9'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0005              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO MUNICIPIO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-OBTER-DESC-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO AREA-RURC88.

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE OF GFCTB0C9  TO WRK-TAM-07-COM-S.
           MOVE WRK-TAM-07-SEM-S       TO COD-MUNIC-RURC88.

           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO             USING AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0732               TO WAMX-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0675               TO WAMX-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0733               TO WAMX-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0734               TO WAMX-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA TRATAR CONSULTA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-CONSULTAR-UF               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2810-ACESSAR-GFCTB0D3.

           MOVE CSGL-UF OF GFCTB0D3        TO WAMX-UF

           PERFORM 2820-OBTER-DESC-UF

           MOVE WRK-AREA-DCLGEN-DCITV002   TO DCITV002.
           MOVE IUF OF DCITV002            TO WAMX-DESC-UF.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA  ACESSAR-GFCTB0D3                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2810-ACESSAR-GFCTB0D3              SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE WAMW-SEQUENCIA         TO CSEQ-AGPTO-CTA    OF GFCTB0D3.
           MOVE WAMW-DATA-INICIO       TO DINIC-PRMSS-PCOTE OF GFCTB0D3.

           EXEC SQL
               SELECT
                   CSGL_UF
               INTO
                   :GFCTB0D3.CSGL-UF
               FROM
                   DB2PRD.PRMSS_GRP_UF
               WHERE
                   CSERVC_TARIF        = :GFCTB0D3.CSERVC-TARIF
               AND CAGPTO_CTA          = :GFCTB0D3.CAGPTO-CTA
               AND CSEQ_AGPTO_CTA      = :GFCTB0D3.CSEQ-AGPTO-CTA
               AND DINIC_PRMSS_PCOTE   = :GFCTB0D3.DINIC-PRMSS-PCOTE

           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS ) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D3'      TO WRK-NOME-TAB
               MOVE 'PRMSS_GRP_UF'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE  0005              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA OBTER DESCRICAO UF                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2820-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CSGL-UF   OF GFCTB0D3  TO CSGL-UF OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0735               TO WAMX-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0736               TO WAMX-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0737               TO WAMX-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO WAMX-ERRO
               MOVE 'S'                TO WAMX-FIM
               MOVE ZEROS              TO WAMX-COD-SQL-ERRO
               MOVE 0738               TO WAMX-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0005'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - DCITV002'      TO WRK-NOME-TAB
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0005              TO WAMX-COD-MSG-ERRO
               MOVE  9                 TO WAMX-ERRO
               MOVE 'GFCT0124'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE  WRK-SQLCA         TO SQLCA
               MOVE  WRK-SQLCA         TO GFCT0M-SQLCA-AREA
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          WRK-TAM-09-COM-S
               MOVE  WRK-TAM-09-SEM-S(7:3)
                                       TO WAMX-COD-SQL-ERRO
               MOVE SPACES             TO WAMX-DESC-MSG-ERRO
               STRING GFCTG3-DESC-MSG  WRK-NOME-TAB
               DELIMITED BY '  '       INTO WAMX-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA ACESSAR GFCTB0C6                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-ACESSAR-GFCTB0C6           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                   CSERVC_TARIF,
                   CAGPTO_CTA,
                   CSEQ_AGPTO_CTA,
                   DINIC_PRMSS_PCOTE,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DFIM_PRMSS_PCOTE,
                   CINDCD_ADSAO_INDVD,
                   CINDCD_AGPTO_TOT,
                   HMANUT_REG,
                   HINCL_REG_SIST,
                   CINDCD_EXCEC_ADSAO,
                   CIDTFD_PRMSS_INDVD,
BI0412             CINDCD_VSLAO_COMP,
BI0810             CPSSOA_SERVC_TARIF
             INTO
                   :GFCTB0C6.CSERVC-TARIF,
                   :GFCTB0C6.CAGPTO-CTA,
                   :GFCTB0C6.CSEQ-AGPTO-CTA,
                   :GFCTB0C6.DINIC-PRMSS-PCOTE,
                   :GFCTB0C6.CFUNC-MANUT-INCL,
                   :GFCTB0C6.CFUNC-MANUT,
                   :GFCTB0C6.DFIM-PRMSS-PCOTE,
                   :GFCTB0C6.CINDCD-ADSAO-INDVD,
                   :GFCTB0C6.CINDCD-AGPTO-TOT,
                   :GFCTB0C6.HMANUT-REG,
                   :GFCTB0C6.HINCL-REG-SIST,
                   :GFCTB0C6.CINDCD-EXCEC-ADSAO,
                   :GFCTB0C6.CIDTFD-PRMSS-INDVD,
BI0412             :GFCTB0C6.CINDCD-VSLAO-COMP,
BI0810             :GFCTB0C6.CPSSOA-SERVC-TARIF
             FROM  DB2PRD.PRMSS_ADSAO_PCOTE
             WHERE CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
              AND  CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA
              AND  CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
              AND  DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0C6'      TO WRK-NOME-TAB
               MOVE 'PRMSS_ADSAO_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0012              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA VALORIZAR FUNCIONARIO                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2950-VALORIZAR-FUNC             SECTION.
      *----------------------------------------------------------------*

           IF  CFUNC-MANUT-INCL OF GFCTB0C6 NOT EQUAL ZEROS
               MOVE CFUNC-MANUT-INCL   OF GFCTB0C6
                                       TO WRK-S9-9
               MOVE WRK-X-7            TO WAMX-COD-FUNC-INCL
               MOVE WRK-9-9            TO WRK-COD-FUNC
               PERFORM 2951-ACESSAR-SARH8000
               MOVE WRK-DESC-FUNCIONARIO
                                       TO WAMX-DESC-FUNC-INCL
           ELSE
               MOVE SPACES             TO WAMX-DESC-FUNC-INCL
           END-IF.

           IF  CFUNC-MANUT OF GFCTB0C6 NOT EQUAL ZEROS
               MOVE CFUNC-MANUT        OF GFCTB0C6
                                       TO WRK-S9-9
               MOVE WRK-X-7            TO WAMX-COD-FUNC-CANCEL
               MOVE WRK-9-9            TO WRK-COD-FUNC
               PERFORM 2951-ACESSAR-SARH8000
               MOVE WRK-DESC-FUNCIONARIO
                                       TO WAMX-DESC-FUNC-CANCEL
           ELSE
               MOVE SPACES             TO WAMX-DESC-FUNC-CANCEL
           END-IF.

           MOVE HINCL-REG-SIST         OF GFCTB0C6
                                       TO WRK-TIMESTAMP
           MOVE WRK-ANO                TO WRK-ANO-DB2
           MOVE WRK-MES                TO WRK-MES-DB2
           MOVE WRK-DIA                TO WRK-DIA-DB2
           MOVE WRK-HORA               TO WRK-HORA-X
           MOVE WRK-MIN                TO WRK-MINU-X
           MOVE WRK-DATA-DB2           TO WAMX-DATA-INCL
           MOVE WRK-TIME-X             TO WAMX-HORA-INCL

           IF  HMANUT-REG OF GFCTB0C6 NOT EQUAL
                                          '0001-01-01-01.01.01.000001'
               MOVE HMANUT-REG         OF GFCTB0C6
                                       TO WRK-TIMESTAMP
               MOVE WRK-ANO            TO WRK-ANO-DB2
               MOVE WRK-MES            TO WRK-MES-DB2
               MOVE WRK-DIA            TO WRK-DIA-DB2
               MOVE WRK-HORA           TO WRK-HORA-X
               MOVE WRK-MIN            TO WRK-MINU-X
               MOVE WRK-DATA-DB2       TO WAMX-DATA-CANCEL
               MOVE WRK-TIME-X         TO WAMX-HORA-CANCEL
           END-IF.

      *----------------------------------------------------------------*
       2950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O NOME DO FUNCIONARIO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2951-ACESSAR-SARH8000           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-FUNC           TO WRK-PESQUISA-ENT.
           MOVE 002                    TO WRK-CAMPO-ENT(1).
           MOVE 086                    TO WRK-TAMANHO-ENT.
           MOVE 'SARH8000'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-ENTRADA
                                             WRK-SAIDA
                                             WRK-ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-COD-RETORNO-SAI     EQUAL ZEROS
               MOVE ZEROS              TO WAMX-ERRO
                                          WAMX-COD-SQL-ERRO
                                          WAMX-COD-MSG-ERRO
               MOVE 'PROCESSAMENTO NORMAL'
                                       TO WAMX-DESC-MSG-ERRO
               MOVE WRK-REGISTRO-SAI(1:40)
                                       TO WRK-DESC-FUNCIONARIO
           ELSE
               MOVE ZEROS              TO RETURN-CODE
               MOVE 'FUNCIONARIO NAO ENCONTRADO'
                                       TO WRK-DESC-FUNCIONARIO
           END-IF.

      *----------------------------------------------------------------*
       2951-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR TABELA GFCTB0D8                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2960-ACESSAR-GFCTB0D8           SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-TARIFA        TO CSERVC-TARIF
                                       OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D8'      TO WRK-NOME-TAB
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  0012              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO RSERVC-TARIF-REDZD
                                       OF GFCTB0D8
           END-IF.

      *----------------------------------------------------------------*
       2960-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR TABELA GFCTB0F3                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2970-ACESSAR-GFCTB0F3           SECTION.
      *----------------------------------------------------------------*

           MOVE WAMW-COD-AGPTO         TO CAGPTO-CTA   OF GFCTB0F3.

           EXEC SQL
               SELECT
                   RAGPTO_CTA
               INTO
                   :GFCTB0F3.RAGPTO-CTA
               FROM DB2PRD.TPO_AGPTO_CTA
               WHERE
                   CAGPTO_CTA          = :GFCTB0F3.CAGPTO-CTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0012'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE ' - GFCTB0D8'      TO WRK-NOME-TAB
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0012              TO WAMX-COD-MSG-ERRO
               PERFORM 1211-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO RAGPTO-CTA
                                       OF GFCTB0F3
           END-IF.

      *----------------------------------------------------------------*
       2970-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
