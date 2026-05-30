      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0354.
       AUTHOR.     JEAN CARLOS GAZZONI.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0354                                    *
      *    PROGRAMADOR.:   JEAN CARLOS GAZZONI      - CPM PATO BRANCO  *
      *    ANALISTA CPM:   FRANCISMARA PAGNONCELLI  - CPM PATO BRANCO  *
      *    ANALISTA....:   MARCELO T. C. BARROS     - PWI / GRUPO 67   *
      *    DATA........:   24/02/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTA MOVIMENTOS DE ENTRADA DE DADOS.    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.CTRL_MOVTO_RECBD       GFCTB033      *
      *                    DB2PRD.CANCT_MOVTO_ORIGE      GFCTB025      *
200705*                    DB2PRD.V01CTRO_CUSTO          ANPRV005      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT3E - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCT3F - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                    *
200705*    GFCT5507 - OBTER CODIGO DE JUNCAO DO FUNC LOGADO            *
200705*    SENH0315 - OBTER JUNCAO.                                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    POOL1470 - CONSISTE DATAS COM SECULO.                       *
      *----------------------------------------------------------------*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR...: JEAN CARLOS GAZZONI      -  CPM PATO BRANCO *
      *    ANALISTA CPM:   FRANCISMARA PAGNONCELLI  - CPM PATO BRANCO  *
      *    ANALISTA....:   RODRIGO PRADO            - PWI / GRUPO 50   *
      *    DATA..........: 10/03/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO......: ALTERAR A FORMATACAO DO CAMPO DENVIO-MOVTO- *
      *      -TARIF DE FILLER PARA WRK-DATA-BARRA-1 E WRK-DATA-BARRA-2 *
      *----------------------------------------------------------------*
      *                       A L T E R A C A O                        *
      *----------------------------------------------------------------*
      *    PROGRAMADOR...: JEAN CARLOS GAZZONI      - CPM PATO BRANCO  *
      *    ANALISTA CPM:   FRANCISMARA PAGNONCELLI  - CPM PATO BRANCO  *
      *    ANALISTA....:   RODRIGO PRADO            - PWI / GRUPO 50   *
      *    DATA..........: 13/04/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO......: MOVIMENTOS DE ENTRADA DE DADOS - CONSULTA.  *
      *================================================================*
AT0522* ALTERACOES:
AT0522*================================================================
AT0522* DATA       NOME          DESCRICAO
AT0522*========== ============   ======================================
AT0522*18/05/2022 ANGELO TROMBIM - ALTERACAO NO ACESSO A TABELA
AT0522*                            ANPRV005 PARA O SERVICO CARO8200.
AT0522*                          - AJUSTE DAS VARIAVEIS REFERENTE A
AT0522*                            TABELA ANPRB005 PARA CARO8200.
      *================================================================*

           EJECT
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0354 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(002)     VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-FINALIZAR                   PIC  X(001) VALUE 'N'.
       77  WRK-ROTINA                      PIC  X(004) VALUE SPACES.
       77  WRK-SITU-MOVTO                  PIC  9(002) VALUE ZEROS.
       77  WRK-TIPO-CONS-SITU              PIC  9(002) VALUE ZEROS.
       77  WRK-MODULO                      PIC  X(008) VALUE SPACES.
       77  WRK-ERRO-DATA                   PIC  X(001) VALUE SPACES.
       77  WRK-ERRO-DATA-INI               PIC  X(001) VALUE SPACES.
       77  WRK-ERRO-DATA-FIM               PIC  X(001) VALUE SPACES.

       01  WRK-SEQ-ARQ                     PIC S9(002)V COMP-3
                                                       VALUE ZEROS.
220605 01  WRK-SEQ-ARQ-F                   PIC S9(002)V COMP-3
                                                       VALUE ZEROS.
       01  WRK-TIPO-MOVTO                  PIC S9(002)V COMP-3
                                                       VALUE ZEROS.

       01  WRK-DATA-DMA.
            03  WRK-DIA                    PIC  X(002) VALUE SPACES.
            03  WRK-DATA-BARRA-1           PIC  X(001) VALUE SPACES.
            03  WRK-MES                    PIC  X(002) VALUE SPACES.
            03  WRK-DATA-BARRA-2           PIC  X(001) VALUE SPACES.
            03  WRK-ANO                    PIC  X(004) VALUE SPACES.

       01  WRK-DATA-INI-AMD.
           03  WRK-ANO-INI                 PIC  X(004) VALUE SPACES.
           03  WRK-MES-INI                 PIC  X(002) VALUE SPACES.
           03  WRK-DIA-INI                 PIC  X(002) VALUE SPACES.

       01  WRK-DATA-FIM-AMD.
           03  WRK-ANO-FIM                 PIC  X(004) VALUE SPACES.
           03  WRK-MES-FIM                 PIC  X(002) VALUE SPACES.
           03  WRK-DIA-FIM                 PIC  X(002) VALUE SPACES.

       01  WRK-DATA-INI-DATE-DB2           PIC  X(010) VALUE SPACES.
       01  FILLER     REDEFINES            WRK-DATA-INI-DATE-DB2.
           03  WRK-DIA-INI-DB2             PIC  X(002).
           03  FILLER                      PIC  X(001).
           03  WRK-MES-INI-DB2             PIC  X(002).
           03  FILLER                      PIC  X(001).
           03  WRK-ANO-INI-DB2             PIC  X(004).

       01  WRK-DATA-FIM-DATE-DB2           PIC  X(010) VALUE SPACES.
       01  FILLER     REDEFINES            WRK-DATA-FIM-DATE-DB2.
           03  WRK-DIA-FIM-DB2             PIC  X(002).
           03  FILLER                      PIC  X(001).
           03  WRK-MES-FIM-DB2             PIC  X(002).
           03  FILLER                      PIC  X(001).
           03  WRK-ANO-FIM-DB2             PIC  X(004).

       01  WRK-FILTRO.
           03  WRK-FILTRO-ROTINA           PIC  X(004) VALUE SPACES.
           03  WRK-FILTRO-DENV-INI         PIC  X(010) VALUE SPACES.
           03  WRK-FILTRO-DENV-FIM         PIC  X(010) VALUE SPACES.
           03  WRK-FILTRO-TIPO-MOVTO       PIC  9(002) VALUE ZEROS.
           03  WRK-FILTRO-SITUACAO-MOVTO   PIC  9(002) VALUE ZEROS.
           03  WRK-FILTRO-TIPO-CONS-SITU   PIC  9(002) VALUE ZEROS.

       01  WRK-PONTEIRO.
           03  WRK-PONTEI-ROTINA           PIC  X(004) VALUE SPACES.
           03  WRK-PONTEI-DENV-INI         PIC  X(010) VALUE SPACES.
           03  WRK-PONTEI-DENV-FIM         PIC  X(010) VALUE SPACES.
           03  WRK-PONTEI-TIPO-MOVTO       PIC  9(002) VALUE ZEROS.
           03  WRK-PONTEI-SITUACAO-MOVTO   PIC  9(002) VALUE ZEROS.
           03  WRK-PONTEI-TIPO-CONS-SITU   PIC  9(002) VALUE ZEROS.
           03  WRK-PONTEI-CSEQ-MOVTO       PIC  9(011) VALUE ZEROS.

       01  WRK-CAD-DATA.
           03  WRK-CAD-DIA                 PIC  9(002) VALUE ZEROS.
           03  WRK-CAD-MES                 PIC  9(002) VALUE ZEROS.
           03  WRK-CAD-ANO                 PIC  9(004) VALUE ZEROS.

AT0522 01  WRK-GEST-CCUSTO                 PIC  9(005) VALUE ZEROS.
AT0522 01  FILLER REDEFINES WRK-GEST-CCUSTO.
AT0522     05  FILLER                      PIC  X(001).
AT0522     05  WRK-GEST-CCUSTO-R           PIC  9(004).
AT0522     05  WRK-GEST-CCUSTO-1 REDEFINES WRK-GEST-CCUSTO-R
AT0522                                     PIC  9(004) COMP-3.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CAMPOS DO MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE     PIC X(001)        VALUE SPACES.
               05  WRK-5000-COD-MSG      PIC 9(004)        VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
               05  WRK-5000-DESC-MSG     PIC X(070)        VALUE SPACES.

           EJECT
200705
200705*----------------------------------------------------------------*
200705 01  FILLER                      PIC  X(050)         VALUE
200705     '*** AREA PARA MODULO GFCT5507 ***'.
200705*----------------------------------------------------------------*
200705
200705 01  WRK-5507-AREA.
200705     03  WRK-5507-AREA-ENVIA.
200705         05  WRK-5507-AMBIENTE     PIC X(001)        VALUE SPACES.
200705         05  WRK-5507-CJUNC-DEPDC  PIC 9(005)        VALUE ZEROS.
200705     03  WRK-5507-AREA-RECEBE.
200705         05  WRK-5507-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
200705         05  WRK-5507-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
200705         05  WRK-5507-MSG-RETORNO  PIC X(045)        VALUE SPACES.
200705         05  WRK-5507-IDTFD-MASTER PIC X(001)        VALUE SPACES.
200705
200705*----------------------------------------------------------------*
200705 01  FILLER                      PIC  X(050)         VALUE
200705     '*** AREA PARA MODULO SENH0315 ***'.
200705*----------------------------------------------------------------*
200705
200705 01  WRK-VERSAO-0315             PIC  X(006)         VALUE
200705     'VRS001'.
200705 01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
200705 01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
200705 01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
200705 01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
200705 01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
200705 01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
200705 01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
200705 01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.
200705
200705 01  WRK-MENSAGEM-0315.
200705     03  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
200705     03  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
200705     03  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
200705
200705 01  WRK-CNPJ-BANCO-0315.
200705     03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
200705     03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
200705     03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
200705
200705     EJECT
200705
AT0522*----------------------------------------------------------------*
AT0522 01  FILLER                      PIC X(050)  VALUE
AT0522     'AREA DE INTERFACE COM SERVICO FUNCIONAL CARO8200'.
AT0522*----------------------------------------------------------------*
AT0522
AT0522 01  WRK-CARO8200                PIC  X(008)         VALUE
AT0522     'CARO8200'.
AT0522
AT0522     COPY 'CAROW820'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB033
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE CAROB001
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB025
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR1-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR1-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <= :WRK-DATA-FIM-DATE-DB2))
220605         AND  (CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ
220605         AND   CNRO_ARQ_MOVTO     <= :WRK-SEQ-ARQ-F )
             ORDER BY  CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR2-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR2-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <= :WRK-DATA-FIM-DATE-DB2))
               AND  (CTRATO_MOVTO       =  :GFCTB033.CTRATO-MOVTO)
220605         AND  (CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ
220605         AND   CNRO_ARQ_MOVTO     <= :WRK-SEQ-ARQ-F)
             ORDER BY  CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR3-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR3-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <= :WRK-DATA-FIM-DATE-DB2))
               AND  (CSIT_MOVTO_RECBD   =  :GFCTB033.CSIT-MOVTO-RECBD)
               AND  (CNRO_ARQ_MOVTO     =  90)
             ORDER  BY CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR4-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR4-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <= :WRK-DATA-FIM-DATE-DB2))
               AND  (CTRATO_MOVTO       =  :GFCTB033.CTRATO-MOVTO)
               AND  (CSIT_MOVTO_RECBD   =  :GFCTB033.CSIT-MOVTO-RECBD)
               AND  (CNRO_ARQ_MOVTO     =  90)
             ORDER BY  CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR5-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR5-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <= :WRK-DATA-FIM-DATE-DB2))
               AND  (CSIT_MOVTO_RECBD   IN ( 01, 02 ))
               AND  (CNRO_ARQ_MOVTO     <> 90)
             ORDER BY  CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR6-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR6-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <= :WRK-DATA-FIM-DATE-DB2))
               AND  (CTRATO_MOVTO       =  :GFCTB033.CTRATO-MOVTO)
               AND  (CSIT_MOVTO_RECBD   IN ( 01, 02 ))
               AND  (CNRO_ARQ_MOVTO     <> 90)
             ORDER BY  CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR7-GFCTB025 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR7-GFCTB025 CURSOR FOR
             SELECT  DENVIO_MOVTO_CANCT,
                     CROTNA_ORIGE_CANCT,
                     CNRO_ARQ_CANCT,
                     CSEQ_CANCT
             FROM    DB2PRD.CANCT_MOVTO_ORIGE
             WHERE  (CROTNA_ORIGE_CANCT =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_CANCT =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_CANCT     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_CANCT >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_CANCT <= :WRK-DATA-FIM-DATE-DB2))
               AND  (CSEQ_CANCT         =  :GFCTB025.CSEQ-CANCT)
             ORDER BY  DENVIO_MOVTO_CANCT,
                       CROTNA_ORIGE_CANCT,
                       CNRO_ARQ_CANCT
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR8-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR8-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND   CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <= :WRK-DATA-FIM-DATE-DB2))
               AND  (CSIT_MOVTO_RECBD   =  :GFCTB033.CSIT-MOVTO-RECBD)
220605         AND  (CNRO_ARQ_MOVTO     >= :WRK-SEQ-ARQ
220605         AND   CNRO_ARQ_MOVTO     <= :WRK-SEQ-ARQ-F)
             ORDER BY  CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DO CURSOR CSR9-GFCTB033 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR9-GFCTB033 CURSOR FOR
             SELECT  DENVIO_MOVTO_TARIF,
                     CROTNA_ORIGE_MOVTO,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE  (CROTNA_ORIGE_MOVTO =  :WRK-ROTINA)
               AND ((DENVIO_MOVTO_TARIF =  :WRK-DATA-INI-DATE-DB2
               AND  CNRO_ARQ_MOVTO      >= :WRK-SEQ-ARQ)
               OR   (DENVIO_MOVTO_TARIF >  :WRK-DATA-INI-DATE-DB2
               AND   DENVIO_MOVTO_TARIF <=
                                        :WRK-DATA-FIM-DATE-DB2))
               AND  (CTRATO_MOVTO       =  :GFCTB033.CTRATO-MOVTO)
               AND  (CSIT_MOVTO_RECBD   =  :GFCTB033.CSIT-MOVTO-RECBD )
             ORDER BY  CROTNA_ORIGE_MOVTO,
                       DENVIO_MOVTO_TARIF,
                       CNRO_ARQ_MOVTO
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0354 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

-INC I#GFCT3E

           EJECT

-INC I#GFCT3F

           EJECT

-INC I#GFCT0M

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCT3E-ENTRADA
                                             GFCT3F-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCT3F-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCT3F-SAIDA.

           PERFORM 110000-MONTAR-AREA-SAIDA.

           PERFORM 120000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE 250                     TO GFCT3F-LL.
           MOVE GFCT3E-TRANSACAO        TO GFCT3F-TRANSACAO.
           MOVE GFCT3E-FUNCAO           TO GFCT3F-FUNCAO.

           IF  GFCT3E-QTDE-OCOR         NOT NUMERIC
               MOVE ZEROS               TO GFCT3F-QTDE-OCOR
           ELSE
               MOVE GFCT3E-QTDE-OCOR    TO GFCT3F-QTDE-OCOR
           END-IF.

           IF  GFCT3E-QTDE-TOT-REG      NOT NUMERIC
               MOVE ZEROS               TO GFCT3F-QTDE-TOT-REG
           ELSE
               MOVE GFCT3E-QTDE-TOT-REG TO GFCT3F-QTDE-TOT-REG
           END-IF.

           MOVE GFCT3E-PONTEIRO         TO GFCT3F-PONTEIRO.
           MOVE GFCT3E-FUNC-BDSCO       TO GFCT3F-FUNC-BDSCO.
           MOVE ZEROS                   TO GFCT3F-ERRO
                                           GFCT3F-COD-SQL-ERRO
                                           GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG.
           MOVE 'N'                     TO GFCT3F-FIM.

           PERFORM  111000-OBTER-DESCRICAO-MSG.

           MOVE WRK-5000-DESC-MSG      TO  GFCT3F-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                            TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'                     TO WRK-MODULO.

           CALL 'POOL0081'                     USING
                                                   WRK-MODULO
                                                   WRK-5000-AREA-ENVIA
                                                   WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO            NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO        EQUAL 01
                   MOVE 9                      TO GFCT3F-ERRO
                   MOVE ZEROS                  TO GFCT3F-COD-SQL-ERRO
                                                  GFCT3F-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG      TO GFCT3F-DESC-MSG-ERRO
               ELSE
               IF  WRK-5000-COD-RETORNO        EQUAL 99
                   MOVE 9                      TO GFCT3F-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO  TO GFCT3F-COD-SQL-ERRO
                   MOVE ZEROS                  TO GFCT3F-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG      TO GFCT3F-DESC-MSG-ERRO
               END-IF
               END-IF
               MOVE SPACES                     TO GFCT3F-DADOS
               INITIALIZE GFCT3F-DADOS
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       120000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT3E-FILTRO              TO WRK-FILTRO.
           MOVE GFCT3E-PONTEIRO            TO WRK-PONTEIRO.

           IF  GFCT3E-ROTNA-ORIG-PT    NOT EQUAL LOW-VALUES AND
                                           NOT EQUAL SPACES
               MOVE WRK-PONTEI-DENV-INI    TO WRK-DATA-DMA
               MOVE WRK-ANO                TO WRK-ANO-INI
               MOVE WRK-MES                TO WRK-MES-INI
               MOVE WRK-DIA                TO WRK-DIA-INI
               MOVE WRK-PONTEI-DENV-FIM    TO WRK-DATA-DMA
               MOVE WRK-ANO                TO WRK-ANO-FIM
               MOVE WRK-MES                TO WRK-MES-FIM
               MOVE WRK-DIA                TO WRK-DIA-FIM
           ELSE
               MOVE WRK-FILTRO-DENV-INI    TO WRK-DATA-DMA
               MOVE WRK-ANO                TO WRK-ANO-INI
               MOVE WRK-MES                TO WRK-MES-INI
               MOVE WRK-DIA                TO WRK-DIA-INI
               MOVE WRK-FILTRO-DENV-FIM    TO WRK-DATA-DMA
               MOVE WRK-ANO                TO WRK-ANO-FIM
               MOVE WRK-MES                TO WRK-MES-FIM
               MOVE WRK-DIA                TO WRK-DIA-FIM
               MOVE WRK-FILTRO             TO WRK-PONTEIRO
           END-IF.

           PERFORM 121000-VALIDAR-DATAS.

           IF  GFCT3E-ROTNA-ORIG           EQUAL SPACES
               MOVE 1                      TO GFCT3F-ERRO-ROTNA-ORIGEM
200705     ELSE
200705         IF GFCT3E-ROTNA-ORIG        NOT EQUAL     'GFCT'
200705            PERFORM 130000-VERIFICA-ACESSO-ROTINA
200705         END-IF
           END-IF.

           IF  WRK-ERRO-DATA-INI           EQUAL 'S'
               MOVE 1                      TO GFCT3F-ERRO-DT-ENV-INI
           END-IF.

           IF  WRK-ERRO-DATA-FIM           EQUAL 'S'
               MOVE 1                      TO GFCT3F-ERRO-DT-ENV-FIM
           END-IF.

           IF  GFCT3E-TIPO-MOVTO           NOT NUMERIC
               MOVE 1                      TO GFCT3F-ERRO-TIPO-MOVTO
           END-IF.

           IF (GFCT3E-SITU-MOVTO           NOT NUMERIC)            OR
              (GFCT3E-SITU-MOVTO           NOT EQUAL 00 AND 01     AND
                                           02 AND 03 AND 04)
               MOVE 1                      TO GFCT3F-ERRO-SIT-MOVTO
           END-IF.

           IF (GFCT3E-TP-CONS              NOT NUMERIC)            OR
              (GFCT3E-TP-CONS              NOT EQUAL 01 AND 02     AND
                                           03 AND 04 AND 50        AND
                                           80 AND 90)
               MOVE 1                      TO GFCT3F-ERRO-TIPO-CONS-SIT
           END-IF.

           IF (GFCT3E-TRANSACAO            EQUAL LOW-VALUES)       OR
              (GFCT3E-TRANSACAO            EQUAL SPACES)           OR
              (GFCT3E-FUNCAO               EQUAL LOW-VALUES)       OR
              (GFCT3E-FUNCAO               EQUAL SPACES)           OR
              (GFCT3E-FILTRO               EQUAL LOW-VALUES)       OR
              (GFCT3E-FILTRO               EQUAL SPACES)           OR
              (GFCT3E-QTDE-OCOR            NOT NUMERIC)            OR
              (GFCT3E-QTDE-TOT-REG         NOT NUMERIC)            OR
              (GFCT3E-FUNC-BDSCO           EQUAL LOW-VALUES)       OR
              (GFCT3E-FUNC-BDSCO           EQUAL SPACES)           OR
              (GFCT3E-FIM                  NOT EQUAL 'S' AND 'N')  OR
              (GFCT3F-ERRO-ROTNA-ORIGEM    EQUAL 1)                OR
              (GFCT3F-ERRO-DT-ENV-INI      EQUAL 1)                OR
              (GFCT3F-ERRO-DT-ENV-FIM      EQUAL 1)                OR
              (GFCT3F-ERRO-TIPO-MOVTO      EQUAL 1)                OR
              (GFCT3F-ERRO-SIT-MOVTO       EQUAL 1)                OR
              (GFCT3F-ERRO-TIPO-CONS-SIT   EQUAL 1)
               MOVE 1                      TO GFCT3F-ERRO
               MOVE ZEROS                  TO GFCT3F-COD-SQL-ERRO
               MOVE 0001                   TO GFCT3F-COD-MSG-ERRO
                                                 WRK-5000-COD-MSG
               MOVE 'S'                    TO GFCT3F-FIM
               PERFORM  111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG      TO GFCT3F-DESC-MSG-ERRO
               PERFORM  300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       121000-VALIDAR-DATAS            SECTION.
      *----------------------------------------------------------------*

           IF   WRK-DATA-INI-AMD       NOT EQUAL SPACES
                MOVE  'N'              TO WRK-ERRO-DATA
                MOVE  WRK-DIA-INI      TO WRK-CAD-DIA
                MOVE  WRK-ANO-INI      TO WRK-CAD-ANO
                MOVE  WRK-MES-INI      TO WRK-CAD-MES
                PERFORM 121100-VALIDAR-POOL1470
            ELSE
                IF  WRK-ERRO-DATA      EQUAL 'S'
                    MOVE 'S'           TO WRK-ERRO-DATA-INI
                END-IF
           END-IF.

           IF   WRK-DATA-FIM-AMD       NOT EQUAL SPACES
                MOVE  'N'              TO WRK-ERRO-DATA
                MOVE  WRK-DIA-FIM      TO WRK-CAD-DIA
                MOVE  WRK-ANO-FIM      TO WRK-CAD-ANO
                MOVE  WRK-MES-FIM      TO WRK-CAD-MES
                PERFORM 121100-VALIDAR-POOL1470
                IF  WRK-ERRO-DATA      EQUAL 'S'
                    MOVE 'S'           TO WRK-ERRO-DATA-FIM
                END-IF
            END-IF.

           IF   WRK-ERRO-DATA-INI      EQUAL 'S' OR
                WRK-ERRO-DATA-FIM      EQUAL 'S'
                GO TO 121000-99-FIM
           END-IF.

           IF   WRK-DATA-INI-AMD       EQUAL SPACES AND
                WRK-DATA-FIM-AMD       NOT EQUAL SPACES
                MOVE   'S'             TO  WRK-ERRO-DATA-INI
                                           WRK-ERRO-DATA-FIM
           END-IF.

           IF   WRK-DATA-INI-AMD       NOT EQUAL SPACES AND
                WRK-DATA-FIM-AMD       NOT EQUAL SPACES
                IF  WRK-DATA-FIM-AMD   LESS WRK-DATA-INI-AMD
                    MOVE 'S'           TO WRK-ERRO-DATA-FIM
                END-IF
           END-IF.

      *----------------------------------------------------------------*
       121000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       121100-VALIDAR-POOL1470         SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1470'             USING WRK-CAD-DATA.

           IF   RETURN-CODE            NOT EQUAL ZEROS
                MOVE 'S'               TO WRK-ERRO-DATA
           END-IF.

      *----------------------------------------------------------------*
       121100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
200705*----------------------------------------------------------------*
200705 130000-VERIFICA-ACESSO-ROTINA   SECTION.
200705*----------------------------------------------------------------*
200705
200705     PERFORM 130100-OBTER-COD-JUNCAO.
200705
200705     PERFORM 130200-ACESSAR-GFCT5507.
200705
200705     IF  WRK-5507-IDTFD-MASTER       EQUAL 'S'
AT0522         PERFORM 1303-SELECT-CAROB001-CUSTO
200705     ELSE
AT0522         PERFORM 1304-SELECT-CAROB001-DEPDC
200705     END-IF.
200705
200705*----------------------------------------------------------------*
200705 130000-99-FIM.                  EXIT.
200705*----------------------------------------------------------------*
200705     EJECT
200705*----------------------------------------------------------------*
200705 130100-OBTER-COD-JUNCAO         SECTION.
200705*----------------------------------------------------------------*
200705
200705     MOVE GFCT3E-FUNC-BDSCO           TO WRK-COD-FUNCIONAL-0315.
200705
200705     CALL 'SENH0315'                  USING WRK-VERSAO-0315
200705                                            WRK-MENSAGEM-0315
200705                                            WRK-COD-FUNCIONAL-0315
200705                                            WRK-NOME-FUNC-0315
200705                                            WRK-COD-BANCO-0315
200705                                            WRK-NOME-BANCO-0315
200705                                            WRK-CNPJ-BANCO-0315
200705                                            WRK-COD-JUNCAO-0315
200705                                            WRK-NOME-DEPTO-0315
200705                                            WRK-SECAO-0315
200705                                            WRK-STATUS-0315.
200705
200705     IF  RETURN-CODE                    NOT EQUAL ZEROS AND 4
200705         MOVE 1                          TO GFCT3F-ERRO
200705         MOVE RETURN-CODE                TO GFCT3F-COD-SQL-ERRO
200705         MOVE 0056                       TO GFCT3F-COD-MSG-ERRO
200705                                            WRK-5000-COD-MSG
200705         PERFORM 111000-OBTER-DESCRICAO-MSG
200705         MOVE WRK-5000-DESC-MSG         TO GFCT3F-DESC-MSG-ERRO
200705         MOVE 'S'                       TO GFCT3F-FIM
200705         INITIALIZE GFCT3F-DADOS
200705         PERFORM    300000-FINALIZAR
200705     END-IF.
200705
200705     IF  RETURN-CODE                    EQUAL 4
200705         EVALUATE  WRK-COD-MENSAGEM-0315
200705
200705             WHEN 001
200705                 MOVE 1                 TO GFCT3F-ERRO
200705                 MOVE ZEROS             TO GFCT3F-COD-SQL-ERRO
200705                 MOVE 0057              TO GFCT3F-COD-MSG-ERRO
200705                                           WRK-5000-COD-MSG
200705                 PERFORM 111000-OBTER-DESCRICAO-MSG
200705                 MOVE WRK-5000-DESC-MSG TO GFCT3F-DESC-MSG-ERRO
200705                 MOVE 'S'               TO GFCT3F-FIM
200705                 PERFORM 300000-FINALIZAR
200705
200705             WHEN 154
200705                 MOVE 1                 TO GFCT3F-ERRO
200705                 MOVE ZEROS             TO GFCT3F-COD-SQL-ERRO
200705                 MOVE 0058              TO GFCT3F-COD-MSG-ERRO
200705                                           WRK-5000-COD-MSG
200705                 PERFORM 111000-OBTER-DESCRICAO-MSG
200705                 MOVE WRK-5000-DESC-MSG TO GFCT3F-DESC-MSG-ERRO
200705                 MOVE 'S'               TO GFCT3F-FIM
200705                 PERFORM 300000-FINALIZAR
200705
200705             WHEN 242
200705                 MOVE 1                 TO GFCT3F-ERRO
200705                 MOVE ZEROS             TO GFCT3F-COD-SQL-ERRO
200705                 MOVE 0059              TO GFCT3F-COD-MSG-ERRO
200705                                           WRK-5000-COD-MSG
200705                 PERFORM 111000-OBTER-DESCRICAO-MSG
200705                 MOVE WRK-5000-DESC-MSG TO GFCT3F-DESC-MSG-ERRO
200705                 MOVE 'S'               TO GFCT3F-FIM
200705                 PERFORM 300000-FINALIZAR
200705
200705             WHEN 009
200705                 MOVE ZEROS             TO GFCT3F-ERRO
200705                                           GFCT3F-COD-SQL-ERRO
200705                                           GFCT3F-COD-MSG-ERRO
200705                                           WRK-5000-COD-MSG
200705                 PERFORM 111000-OBTER-DESCRICAO-MSG
200705                 MOVE WRK-5000-DESC-MSG TO GFCT3F-DESC-MSG-ERRO
200705                 MOVE 'S'               TO GFCT3F-FIM
200705                 PERFORM 300000-FINALIZAR
200705
200705             WHEN 138
200705                 MOVE 1                 TO GFCT3F-ERRO
200705                 MOVE ZEROS             TO GFCT3F-COD-SQL-ERRO
200705                 MOVE 0060              TO GFCT3F-COD-MSG-ERRO
200705                                           WRK-5000-COD-MSG
200705                 PERFORM 111000-OBTER-DESCRICAO-MSG
200705                 MOVE WRK-5000-DESC-MSG TO GFCT3F-DESC-MSG-ERRO
200705                 MOVE 'S'               TO GFCT3F-FIM
200705                 PERFORM 300000-FINALIZAR
200705         END-EVALUATE
200705     END-IF.
200705
200705*----------------------------------------------------------------*
200705 130100-99-FIM.                  EXIT.
200705*----------------------------------------------------------------*
200705     EJECT
200705
200705*----------------------------------------------------------------*
200705 130200-ACESSAR-GFCT5507         SECTION.
200705*----------------------------------------------------------------*
200705
200705     MOVE 'O'                          TO WRK-5507-AMBIENTE.
200705     MOVE WRK-COD-JUNCAO-0315          TO WRK-5507-CJUNC-DEPDC.
200705
200705     MOVE 'GFCT5507'                   TO WRK-MODULO.
200705
200705     CALL 'POOL0081'                   USING WRK-MODULO
200705                                             WRK-5507-AREA.
200705
200705     IF  WRK-5507-COD-RETORNO          NOT EQUAL ZEROS
200705         IF  WRK-5507-COD-RETORNO      EQUAL 01
200705             MOVE 1                    TO GFCT3F-ERRO
200705             MOVE ZEROS                TO GFCT3F-COD-SQL-ERRO
200705                                          GFCT3F-COD-MSG-ERRO
200705             MOVE WRK-5507-MSG-RETORNO TO GFCT3F-DESC-MSG-ERRO
200705         ELSE
200705             IF  WRK-5507-COD-RETORNO  EQUAL 99
200705                 MOVE 1                TO GFCT3F-ERRO
200705                 MOVE ZEROS            TO GFCT3F-COD-MSG-ERRO
200705                 MOVE WRK-5507-COD-SQL-ERRO
200705                                       TO GFCT3F-COD-SQL-ERRO
200705                 MOVE WRK-5507-MSG-RETORNO
200705                                       TO GFCT3F-DESC-MSG-ERRO
200705             END-IF
200705         END-IF
200705         INITIALIZE GFCT3F-DADOS
200705         MOVE   'S'                    TO GFCT3F-FIM
200705         PERFORM 300000-FINALIZAR
200705     END-IF.
200705
200705*----------------------------------------------------------------*
200705 130200-99-FIM.                  EXIT.
200705*----------------------------------------------------------------*
200705     EJECT
200705
200705*----------------------------------------------------------------*
AT0522 1303-SELECT-CAROB001-CUSTO      SECTION.
200705*----------------------------------------------------------------*
200705
AT0522     INITIALIZE                       CARO01-COMUAREA.

AT0522     MOVE 'S'                     TO  CARO01-ACAO.
AT0522     MOVE 01                      TO  CARO01-TIPO-PESQUISA.
AT0522     MOVE GFCT3E-ROTNA-ORIG       TO  CARO01-CCTRO-CUSTO.
200705
AT0522     CALL  WRK-CARO8200 USING CARO01-COMUAREA.
200705
AT0522     IF (CARO01-COD-RETORNO      NOT EQUAL ZEROS AND 01  )
200705         INITIALIZE GFCT0M-ERRO-SQL
200705         MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
200705         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
AT0522         MOVE 'TCARO_ATIV      '  TO GFCT0M-NOME-TAB
AT0522         MOVE 'CARO8200'          TO GFCT0M-COMANDO-SQL
AT0522         MOVE CARO01-COD-RETORNO  TO GFCT0M-SQLCODE
200705         MOVE '0005'              TO GFCT0M-LOCAL
200705         MOVE SQLCA               TO GFCT0M-SQLCA-AREA
200705         MOVE 9                   TO GFCT3F-ERRO
AT0522         MOVE CARO01-COD-RETORNO  TO GFCT3F-COD-SQL-ERRO
200705         MOVE 0005                TO GFCT3F-COD-MSG-ERRO
200705                                     WRK-5000-COD-MSG
200705         PERFORM 111000-OBTER-DESCRICAO-MSG
200705         STRING WRK-5000-DESC-MSG
AT0522                ' - CARO8200'
200705         DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
200705         MOVE   'S'                    TO GFCT3F-FIM
200705         PERFORM 300000-FINALIZAR
200705     END-IF.
200705
AT0522     IF  CARO01-COD-RETORNO      EQUAL 01
200705         MOVE 1                     TO GFCT3F-ERRO
AT0522         MOVE CARO01-COD-RETORNO    TO GFCT3F-COD-SQL-ERRO
200705         MOVE 0061                  TO GFCT3F-COD-MSG-ERRO
200705                                       WRK-5000-COD-MSG
200705         PERFORM 111000-OBTER-DESCRICAO-MSG
200705         MOVE WRK-5000-DESC-MSG  TO GFCT3F-DESC-MSG-ERRO
200705         INITIALIZE GFCT3F-DADOS
200705         MOVE   'S'                    TO GFCT3F-FIM
200705         PERFORM 300000-FINALIZAR
200705     END-IF.
200705
200705*----------------------------------------------------------------*
200705 1303-99-FIM.                    EXIT.
200705*----------------------------------------------------------------*
200705
200705*----------------------------------------------------------------*
AT0522 1304-SELECT-CAROB001-DEPDC    SECTION.
200705*----------------------------------------------------------------*
200705
AT0522     MOVE WRK-COD-JUNCAO-0315    TO WRK-GEST-CCUSTO.
AT0522     MOVE WRK-GEST-CCUSTO-1      TO GEST-CCUSTO  OF CAROB001.
AT0522     MOVE GFCT3E-ROTNA-ORIG      TO CCUSTO-ASSOC OF CAROB001.
200705
200705     EXEC SQL
200705       SELECT
AT0522             CCUSTO_ASSOC,
AT0522             GEST_CCUSTO
200705       INTO
AT0522             :CAROB001.CCUSTO-ASSOC,
AT0522             :CAROB001.GEST-CCUSTO
AT0522       FROM   DB2PRD.TCARO_ATIV
200705       WHERE
AT0522             CCUSTO_ASSOC       = :CAROB001.CCUSTO-ASSOC AND
AT0522             GEST_CCUSTO        = :CAROB001.GEST-CCUSTO
200705     END-EXEC.
200705
200705     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
200705        (SQLWARN0                EQUAL 'W'               )
200705         INITIALIZE GFCT0M-ERRO-SQL
200705         MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
200705         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
AT0522         MOVE 'TCARO_ATIV   '     TO GFCT0M-NOME-TAB
AT0522         MOVE 'CAROB001'          TO GFCT0M-COMANDO-SQL
200705         MOVE SQLCODE             TO GFCT0M-SQLCODE
200705         MOVE '0007'              TO GFCT0M-LOCAL
200705         MOVE SQLCA               TO GFCT0M-SQLCA-AREA
200705         MOVE 9                   TO GFCT3F-ERRO
200705         MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
200705         MOVE 0005                TO GFCT3F-COD-MSG-ERRO
200705                                     WRK-5000-COD-MSG
200705         PERFORM 111000-OBTER-DESCRICAO-MSG
200705         STRING WRK-5000-DESC-MSG
AT0522                ' - CAROB001'
200705         DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
200705         MOVE   'S'                    TO GFCT3F-FIM
200705         PERFORM 300000-FINALIZAR
200705     END-IF.
200705
200705     IF  SQLCODE                 EQUAL +100
200705         MOVE 1                     TO GFCT3F-ERRO
200705         MOVE SQLCODE               TO GFCT3F-COD-SQL-ERRO
200705         MOVE 0061                  TO GFCT3F-COD-MSG-ERRO
200705                                       WRK-5000-COD-MSG
200705         PERFORM 111000-OBTER-DESCRICAO-MSG
200705         MOVE WRK-5000-DESC-MSG  TO GFCT3F-DESC-MSG-ERRO
200705         INITIALIZE GFCT3F-DADOS
200705         MOVE   'S'                    TO GFCT3F-FIM
200705         PERFORM 300000-FINALIZAR
200705     END-IF.
200705
200705*----------------------------------------------------------------*
AT0522 1304-99-FIM.                    EXIT.
200705*----------------------------------------------------------------*
200705     EJECT
      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3E-ROTNA-ORIG-PT    NOT EQUAL LOW-VALUES AND
                                       NOT EQUAL SPACES
               MOVE  GFCT3E-ROTNA-ORIG-PT  TO WRK-ROTINA
               MOVE  GFCT3E-TIPO-MOVTO-PT  TO WRK-TIPO-MOVTO
               MOVE  GFCT3E-SITU-MOVTO-PT  TO WRK-SITU-MOVTO
               MOVE  GFCT3E-TP-CONS-PT     TO WRK-TIPO-CONS-SITU
               MOVE  GFCT3E-CSEQ-MOVTO-PT  TO WRK-SEQ-ARQ
           ELSE
               MOVE  GFCT3E-ROTNA-ORIG     TO WRK-ROTINA
               MOVE  GFCT3E-TIPO-MOVTO     TO WRK-TIPO-MOVTO
               MOVE  GFCT3E-SITU-MOVTO     TO WRK-SITU-MOVTO
               MOVE  GFCT3E-TP-CONS        TO WRK-TIPO-CONS-SITU
               MOVE  ZEROS                 TO WRK-SEQ-ARQ
           END-IF.

           IF  WRK-DATA-INI-AMD          EQUAL   SPACES
               MOVE '01.01.0001'         TO WRK-DATA-INI-DATE-DB2
           ELSE
               MOVE  WRK-ANO-INI         TO WRK-ANO-INI-DB2
               MOVE  WRK-MES-INI         TO WRK-MES-INI-DB2
               MOVE  WRK-DIA-INI         TO WRK-DIA-INI-DB2
               MOVE  '.'                 TO WRK-DATA-INI-DATE-DB2(3:1)
                                            WRK-DATA-INI-DATE-DB2(6:1)
           END-IF.

           IF  WRK-DATA-FIM-AMD          EQUAL SPACES
               MOVE '31.12.9999'         TO WRK-DATA-FIM-DATE-DB2
           ELSE
               MOVE  WRK-ANO-FIM         TO WRK-ANO-FIM-DB2
               MOVE  WRK-MES-FIM         TO WRK-MES-FIM-DB2
               MOVE  WRK-DIA-FIM         TO WRK-DIA-FIM-DB2
               MOVE  '.'                 TO WRK-DATA-FIM-DATE-DB2(3:1)
                                            WRK-DATA-FIM-DATE-DB2(6:1)
           END-IF.

           IF  WRK-TIPO-MOVTO              NOT EQUAL ZEROS
               PERFORM 210000-TRATAR-ESPECIFICO
               GO                          TO  200000-99-FIM
           END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 01 OR 02 OR 04
               PERFORM 220000-TRATAR-CSR3-GFCTB033
           END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 03
               PERFORM 212000-TRATAR-CSR7-GFCTB025
           END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 50
               PERFORM 230000-TRATAR-CSR5-GFCTB033
           END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 80
220605         MOVE 89                     TO    WRK-SEQ-ARQ-F
220605         IF  WRK-SITU-MOVTO          EQUAL ZEROS
220605             PERFORM 240000-TRATAR-CSR1-GFCTB033
220605         ELSE
220605             IF WRK-SITU-MOVTO       EQUAL 03
220605                PERFORM 212000-TRATAR-CSR7-GFCTB025
220605             ELSE
220605                PERFORM 250000-TRATAR-CSR8-GFCTB033
220605             END-IF
220605         END-IF
220605     END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 90
220605         MOVE 90                     TO    WRK-SEQ-ARQ
220605         MOVE 90                     TO    WRK-SEQ-ARQ-F
               PERFORM 240000-TRATAR-CSR1-GFCTB033
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-TRATAR-ESPECIFICO        SECTION.
      *----------------------------------------------------------------*


           IF  WRK-TIPO-CONS-SITU          EQUAL 01 OR 02 OR 04
               PERFORM 211000-TRATAR-CSR4-GFCTB033
           END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 03
               PERFORM 212000-TRATAR-CSR7-GFCTB025
           END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 50
               PERFORM 213000-TRATAR-CSR6-GFCTB033
           END-IF.


           IF  WRK-TIPO-CONS-SITU          EQUAL 80
220605         MOVE 89                     TO    WRK-SEQ-ARQ-F
220605         IF  WRK-SITU-MOVTO          EQUAL ZEROS
220605             PERFORM 214000-TRATAR-CSR2-GFCTB033
220605         ELSE
220605             IF WRK-SITU-MOVTO       EQUAL 03
220605                PERFORM 212000-TRATAR-CSR7-GFCTB025
220605             ELSE
220605                PERFORM 215000-TRATAR-CSR9-GFCTB033
220605             END-IF
220605         END-IF

           END-IF.

           IF  WRK-TIPO-CONS-SITU          EQUAL 90
220605         MOVE 90                     TO    WRK-SEQ-ARQ
220605         MOVE 90                     TO    WRK-SEQ-ARQ-F
               PERFORM 214000-TRATAR-CSR2-GFCTB033
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-TRATAR-CSR4-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-TIPO-CONS-SITU
               WHEN 01
                   MOVE 01             TO WRK-SITU-MOVTO
               WHEN 02
                   MOVE 02             TO WRK-SITU-MOVTO
               WHEN 04
                   MOVE 04             TO WRK-SITU-MOVTO
           END-EVALUATE.

           PERFORM 211100-ABRIR-CSR4-GFCTB033.

           PERFORM 211200-LER-CSR4-GFCTB033.

           PERFORM 211300-PROCESSAR-CSR4-GFCTB033
             VARYING IND-1             FROM 1 BY 1
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 211400-FECHAR-CSR4-GFCTB033.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211100-ABRIR-CSR4-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-TIPO-MOVTO         TO CTRATO-MOVTO
                                       OF GFCTB033.
           MOVE WRK-SITU-MOVTO         TO CSIT-MOVTO-RECBD
                                       OF GFCTB033.

           EXEC SQL
             OPEN CSR4-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
               MOVE '0010'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCT3F-ERRO
               MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
               MOVE 0005                TO GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       211100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211200-LER-CSR4-GFCTB033        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR4-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0006               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT3F-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCT3F-ERRO
                                          GFCT3F-COD-SQL-ERRO
                   MOVE 0007           TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       211200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211300-PROCESSAR-CSR4-GFCTB033  SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 ).

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB033
                                       TO WRK-DATA-DMA.
           MOVE '/'                    TO WRK-DATA-BARRA-1.
           MOVE '/'                    TO WRK-DATA-BARRA-2.
           MOVE WRK-DATA-DMA           TO GFCT3F-DT-ENVIO-MOVTO(IND-1).
           MOVE CNRO-ARQ-MOVTO         OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1).
           MOVE CTRATO-MOVTO           OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1).
           MOVE CIDTFD-MOVTO-CRRTT     OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1).
           MOVE QEVNTO-RECBD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1).
           MOVE QEVNTO-VALDD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1).
           MOVE CSIT-MOVTO-RECBD       OF GFCTB033
                                       TO
                                       GFCT3F-SITUACAO-MOVTO-RET(IND-1).

           ADD 1                       TO GFCT3F-QTDE-TOT-REG.

           PERFORM 211200-LER-CSR4-GFCTB033.

           IF   WRK-FINALIZAR          EQUAL 'S'
                GO TO 211300-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
           ELSE
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       211300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211400-FECHAR-CSR4-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR4-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       211400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212000-TRATAR-CSR7-GFCTB025     SECTION.
      *----------------------------------------------------------------*

           PERFORM 212100-ABRIR-CSR7-GFCTB025.

           PERFORM 212200-LER-CSR7-GFCTB025.

220605     MOVE  0                     TO      IND-1.

           PERFORM 212300-PROCESSAR-CSR7-GFCTB025
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 212400-FECHAR-CSR7-GFCTB025.

      *----------------------------------------------------------------*
       212000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212100-ABRIR-CSR7-GFCTB025.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO CSEQ-CANCT
                                       OF GFCTB025.

           EXEC SQL
             OPEN CSR7-GFCTB025
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'CANCT_MOVTO_ORIGE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
               MOVE '0040'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCT3F-ERRO
               MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
               MOVE 0005                TO GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB025'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       212100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212200-LER-CSR7-GFCTB025       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR7-GFCTB025 INTO
                   :GFCTB025.DENVIO-MOVTO-CANCT,
                   :GFCTB025.CROTNA-ORIGE-CANCT,
                   :GFCTB025.CNRO-ARQ-CANCT,
                   :GFCTB025.CSEQ-CANCT
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W'               )
               MOVE SPACES                 TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'             TO GFCT0M-PROGRAMA
               MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'CANCT_MOVTO_ORIGE'    TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
               MOVE '0050'                 TO GFCT0M-LOCAL
               MOVE SQLCA                  TO GFCT0M-SQLCA-AREA
               MOVE 9                      TO GFCT3F-ERRO
               MOVE SQLCODE                TO GFCT3F-COD-SQL-ERRO
               MOVE 0006                   TO GFCT3F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB025'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCT3F-FIM
               IF  IND-1                   EQUAL ZEROS
                   MOVE ZEROS              TO GFCT3F-ERRO
                                              GFCT3F-COD-SQL-ERRO
                   MOVE 0007               TO GFCT3F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       212200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212300-PROCESSAR-CSR7-GFCTB025  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TIPO-MOVTO          EQUAL ZEROS
               PERFORM 212310-ACESSAR-MVTO-QUALQUER
           ELSE
               PERFORM 212320-ACESSAR-MVTO-ESPECIFICO
           END-IF.

220605     IF  SQLCODE                 NOT EQUAL ZEROS
220605         PERFORM 212200-LER-CSR7-GFCTB025
220605         IF WRK-FINALIZAR        EQUAL     'S'
220605            GO TO 212300-99-FIM
220605         ELSE
220605            GO TO 212300-PROCESSAR-CSR7-GFCTB025
220605         END-IF
220605     END-IF.

220605     ADD +1                      TO      IND-1.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     OF GFCTB033
                                       TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           ELSE
               COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 )
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033
                                       TO WRK-DATA-DMA
               MOVE '/'                TO WRK-DATA-BARRA-1
               MOVE '/'                TO WRK-DATA-BARRA-2
               MOVE WRK-DATA-DMA       TO GFCT3F-DT-ENVIO-MOVTO(IND-1)
               MOVE CNRO-ARQ-MOVTO     OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1)
               MOVE CTRATO-MOVTO       OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1)
               MOVE CIDTFD-MOVTO-CRRTT OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1)
               MOVE QEVNTO-RECBD       OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1)
               MOVE QEVNTO-VALDD       OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1)
220605         MOVE 3                  TO
                                       GFCT3F-SITUACAO-MOVTO-RET(IND-1)
               ADD 1                   TO GFCT3F-QTDE-TOT-REG
               PERFORM 212200-LER-CSR7-GFCTB025
           END-IF.

      *----------------------------------------------------------------*
       212300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212310-ACESSAR-MVTO-QUALQUER    SECTION.
      *----------------------------------------------------------------*

           INSPECT DENVIO-MOVTO-CANCT REPLACING ALL '/' BY '.'.
           MOVE DENVIO-MOVTO-CANCT    OF GFCTB025
                                      TO DENVIO-MOVTO-TARIF OF GFCTB033.
           MOVE CROTNA-ORIGE-CANCT    OF GFCTB025
                                      TO CROTNA-ORIGE-MOVTO OF GFCTB033.
           MOVE CNRO-ARQ-CANCT        OF GFCTB025
                                      TO CNRO-ARQ-MOVTO     OF GFCTB033.
           MOVE 4                     TO CSIT-MOVTO-RECBD   OF GFCTB033.

           EXEC SQL
             SELECT  DENVIO_MOVTO_TARIF,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             INTO  :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
               FROM  DB2PRD.CTRL_MOVTO_RECBD
               WHERE DENVIO_MOVTO_TARIF = :GFCTB033.DENVIO-MOVTO-TARIF
                 AND CROTNA_ORIGE_MOVTO = :GFCTB033.CROTNA-ORIGE-MOVTO
                 AND CNRO_ARQ_MOVTO     = :GFCTB033.CNRO-ARQ-MOVTO
                 AND CSIT_MOVTO_RECBD  <> :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       212310-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212320-ACESSAR-MVTO-ESPECIFICO  SECTION.
      *----------------------------------------------------------------*

           INSPECT DENVIO-MOVTO-CANCT REPLACING ALL '/' BY '.'.
           MOVE DENVIO-MOVTO-CANCT    OF GFCTB025
                                      TO DENVIO-MOVTO-TARIF OF GFCTB033.
           MOVE CROTNA-ORIGE-CANCT    OF GFCTB025
                                      TO CROTNA-ORIGE-MOVTO OF GFCTB033.
           MOVE CNRO-ARQ-CANCT        OF GFCTB025
                                      TO CNRO-ARQ-MOVTO     OF GFCTB033.
           MOVE 4                     TO CSIT-MOVTO-RECBD   OF GFCTB033.

           EXEC SQL
             SELECT  DENVIO_MOVTO_TARIF,
                     CNRO_ARQ_MOVTO,
                     CTRATO_MOVTO,
                     CIDTFD_MOVTO_CRRTT,
                     QEVNTO_RECBD,
                     QEVNTO_VALDD,
                     CSIT_MOVTO_RECBD
             INTO   :GFCTB033.DENVIO-MOVTO-TARIF,
                    :GFCTB033.CNRO-ARQ-MOVTO,
                    :GFCTB033.CTRATO-MOVTO,
                    :GFCTB033.CIDTFD-MOVTO-CRRTT,
                    :GFCTB033.QEVNTO-RECBD,
                    :GFCTB033.QEVNTO-VALDD,
                    :GFCTB033.CSIT-MOVTO-RECBD
             FROM    DB2PRD.CTRL_MOVTO_RECBD
             WHERE   DENVIO_MOVTO_TARIF = :GFCTB033.DENVIO-MOVTO-TARIF
                 AND CROTNA_ORIGE_MOVTO = :GFCTB033.CROTNA-ORIGE-MOVTO
                 AND CNRO_ARQ_MOVTO     = :GFCTB033.CNRO-ARQ-MOVTO
                 AND CTRATO_MOVTO       = :WRK-TIPO-MOVTO
                 AND CSIT_MOVTO_RECBD  <> :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       212320-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       212400-FECHAR-CSR7-GFCTB025     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR7-GFCTB025
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS) OR
              (SQLWARN0                    EQUAL 'W'      )
               MOVE SPACES                 TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'             TO GFCT0M-PROGRAMA
               MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'CANCT_MOVTO_ORIGE'    TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
               MOVE '0080'                 TO GFCT0M-LOCAL
               MOVE SQLCA                  TO GFCT0M-SQLCA-AREA
               MOVE 9                      TO GFCT3F-ERRO
               MOVE SQLCODE                TO GFCT3F-COD-SQL-ERRO
               MOVE 0011                   TO GFCT3F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB025'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       212400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213000-TRATAR-CSR6-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           PERFORM 213100-ABRIR-CSR6-GFCTB033.

           PERFORM 213200-LER-CSR6-GFCTB033.

           MOVE  ZEROS                 TO  IND-1.

           PERFORM 213300-PROCESSAR-CSR6-GFCTB033
CPM**********VARYING IND-1             FROM 1 BY 1
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 213400-FECHAR-CSR6-GFCTB033.

      *----------------------------------------------------------------*
       213000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213100-ABRIR-CSR6-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-TIPO-MOVTO         TO CTRATO-MOVTO
                                       OF GFCTB033.

           EXEC SQL
             OPEN CSR6-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
               MOVE '0090'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCT3F-ERRO
               MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
               MOVE 0005                TO GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       213100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213200-LER-CSR6-GFCTB033        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR6-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0006               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT3F-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCT3F-ERRO
                                          GFCT3F-COD-SQL-ERRO
                   MOVE 0007           TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       213200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213300-PROCESSAR-CSR6-GFCTB033  SECTION.
      *----------------------------------------------------------------*

           PERFORM 233100-ACESSAR-GFCTB025-CANC.

           IF  SQLCODE             EQUAL +100
               ADD 1                   TO IND-1
               COMPUTE GFCT3F-LL  =  ( GFCT3F-LL   +  38 )
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033
                                       TO WRK-DATA-DMA
               MOVE '/'                TO WRK-DATA-BARRA-1
               MOVE '/'                TO WRK-DATA-BARRA-2
               MOVE WRK-DATA-DMA       TO GFCT3F-DT-ENVIO-MOVTO(IND-1)
               MOVE CNRO-ARQ-MOVTO     OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1)
               MOVE CTRATO-MOVTO       OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1)
               MOVE CIDTFD-MOVTO-CRRTT OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1)
               MOVE QEVNTO-RECBD       OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1)
               MOVE QEVNTO-VALDD       OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1)
               MOVE CSIT-MOVTO-RECBD   OF GFCTB033
                                       TO
                                       GFCT3F-SITUACAO-MOVTO-RET(IND-1)
               ADD 1                   TO GFCT3F-QTDE-TOT-REG
           END-IF.

           PERFORM 213200-LER-CSR6-GFCTB033.

           IF   WRK-FINALIZAR          EQUAL 'S'
                GO TO 213300-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
           ELSE
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       213300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       213400-FECHAR-CSR6-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR6-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       213400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       214000-TRATAR-CSR2-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           PERFORM 214100-ABRIR-CSR2-GFCTB033.

           PERFORM 214200-LER-CSR2-GFCTB033.

           PERFORM 214300-PROCESSAR-CSR2-GFCTB033
             VARYING IND-1             FROM 1 BY 1
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 214400-FECHAR-CSR2-GFCTB033.

      *----------------------------------------------------------------*
       214000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       214100-ABRIR-CSR2-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-TIPO-MOVTO         TO CTRATO-MOVTO
                                       OF GFCTB033.


           EXEC SQL
             OPEN CSR2-GFCTB033
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS) OR
               (SQLWARN0                   EQUAL 'W'      )
                MOVE SPACES                TO GFCT0M-ERRO-SQL
                INITIALIZE GFCT0M-ERRO-SQL
                MOVE 'GFCT0354'            TO GFCT0M-PROGRAMA
                MOVE 'DB2'                 TO GFCT0M-TIPO-ACESSO
                MOVE 'CTRL_MOVTO_RECBD'    TO GFCT0M-NOME-TAB
                MOVE 'OPEN'                TO GFCT0M-COMANDO-SQL
                MOVE SQLCODE               TO GFCT0M-SQLCODE
                MOVE '0120'                TO GFCT0M-LOCAL
                MOVE SQLCA                 TO GFCT0M-SQLCA-AREA
                MOVE 9                     TO GFCT3F-ERRO
                MOVE SQLCODE               TO GFCT3F-COD-SQL-ERRO
                MOVE 0005                  TO GFCT3F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                PERFORM 111000-OBTER-DESCRICAO-MSG
                STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
                DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
                PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       214100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       214200-LER-CSR2-GFCTB033    SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR2-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0006               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT3F-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCT3F-ERRO
                                          GFCT3F-COD-SQL-ERRO
                   MOVE 0007           TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       214200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       214300-PROCESSAR-CSR2-GFCTB033  SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 ).

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB033
                                       TO WRK-DATA-DMA.
           MOVE '/'                    TO WRK-DATA-BARRA-1.
           MOVE '/'                    TO WRK-DATA-BARRA-2.
           MOVE WRK-DATA-DMA           TO GFCT3F-DT-ENVIO-MOVTO(IND-1).
           MOVE CNRO-ARQ-MOVTO         OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1).
           MOVE CTRATO-MOVTO           OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1).
           MOVE CIDTFD-MOVTO-CRRTT     OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1).
           MOVE QEVNTO-RECBD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1).
           MOVE QEVNTO-VALDD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1).

220605     IF  (CSIT-MOVTO-RECBD       OF GFCTB033
220605                                 EQUAL     1 OR 2)  AND
220605         (CNRO-ARQ-MOVTO         OF GFCTB033
220605                                 NOT EQUAL     90)
220605          PERFORM 233100-ACESSAR-GFCTB025-CANC
220605          IF SQLCODE             EQUAL     ZEROS
220605             MOVE 3              TO
220605                                 GFCT3F-SITUACAO-MOVTO-RET(IND-1)
220605          ELSE
220605             MOVE CSIT-MOVTO-RECBD         OF GFCTB033
220605                                 TO
220605                                 GFCT3F-SITUACAO-MOVTO-RET(IND-1)
220605          END-IF
220605     ELSE
220605             MOVE CSIT-MOVTO-RECBD         OF GFCTB033
220605                                 TO
220605                                 GFCT3F-SITUACAO-MOVTO-RET(IND-1)
220605     END-IF.

           ADD 1                       TO GFCT3F-QTDE-TOT-REG.

           PERFORM 214200-LER-CSR2-GFCTB033.

           IF   WRK-FINALIZAR          EQUAL 'S'
                GO TO 214300-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
           ELSE
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       214300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       214400-FECHAR-CSR2-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR2-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       214400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       215000-TRATAR-CSR9-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-TIPO-CONS-SITU
               WHEN 01
                   MOVE 01             TO WRK-SITU-MOVTO
               WHEN 02
                   MOVE 02             TO WRK-SITU-MOVTO
               WHEN 04
                   MOVE 04             TO WRK-SITU-MOVTO
           END-EVALUATE.

           PERFORM 215100-ABRIR-CSR9-GFCTB033.

           PERFORM 215200-LER-CSR9-GFCTB033.

220605     MOVE    0                   TO   IND-1.

           PERFORM 215300-PROCESSAR-CSR9-GFCTB033
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 215400-FECHAR-CSR9-GFCTB033.

      *----------------------------------------------------------------*
       215000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       215100-ABRIR-CSR9-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-TIPO-MOVTO         TO CTRATO-MOVTO
                                       OF GFCTB033.
           MOVE WRK-SITU-MOVTO         TO CSIT-MOVTO-RECBD
                                       OF GFCTB033.

           EXEC SQL
             OPEN CSR9-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
               MOVE '0150'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCT3F-ERRO
               MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
               MOVE 0005                TO GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       215100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       215200-LER-CSR9-GFCTB033        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR9-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0006               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT3F-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCT3F-ERRO
                                          GFCT3F-COD-SQL-ERRO
                   MOVE 0007           TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       215200-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       215300-PROCESSAR-CSR9-GFCTB033  SECTION.
      *----------------------------------------------------------------*

220605     IF   CSIT-MOVTO-RECBD       OF GFCTB033
220605                                 EQUAL     1 OR 2
220605          PERFORM 233100-ACESSAR-GFCTB025-CANC
220605          IF SQLCODE             EQUAL     ZEROS
220605             GO TO 215300-LE-CURSOR
220605          END-IF
220605     END-IF.

220605     ADD  +1                     TO  IND-1.

           COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 ).

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB033
                                       TO WRK-DATA-DMA.
           MOVE '/'                    TO WRK-DATA-BARRA-1.
           MOVE '/'                    TO WRK-DATA-BARRA-2.
           MOVE WRK-DATA-DMA           TO GFCT3F-DT-ENVIO-MOVTO(IND-1).
           MOVE CNRO-ARQ-MOVTO         OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1).
           MOVE CTRATO-MOVTO           OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1).
           MOVE CIDTFD-MOVTO-CRRTT     OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1).
           MOVE QEVNTO-RECBD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1).
           MOVE QEVNTO-VALDD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1).

220605     MOVE CSIT-MOVTO-RECBD       OF GFCTB033
220605                                 TO
220605                                 GFCT3F-SITUACAO-MOVTO-RET(IND-1).

           ADD 1                       TO GFCT3F-QTDE-TOT-REG.

220605 215300-LE-CURSOR.

           PERFORM 215200-LER-CSR9-GFCTB033

           IF   WRK-FINALIZAR          EQUAL 'S'
                GO TO 215300-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
           ELSE
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       215300-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       215400-FECHAR-CSR9-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR9-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       215400-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       220000-TRATAR-CSR3-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-TIPO-CONS-SITU
               WHEN 01
                   MOVE 01             TO WRK-SITU-MOVTO
               WHEN 02
                   MOVE 02             TO WRK-SITU-MOVTO
               WHEN 04
                   MOVE 04             TO WRK-SITU-MOVTO
           END-EVALUATE.

           PERFORM 221000-ABRIR-CSR3-GFCTB033.

           PERFORM 222000-LER-CSR3-GFCTB033.

           PERFORM 223000-PROCESSAR-CSR3-GFCTB033
             VARYING IND-1             FROM 1 BY 1
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 224000-FECHAR-CSR3-GFCTB033.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       221000-ABRIR-CSR3-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-SITU-MOVTO         TO CSIT-MOVTO-RECBD
                                       OF GFCTB033.

           EXEC SQL
             OPEN CSR3-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN  '            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
               MOVE '0180'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCT3F-ERRO
               MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
               MOVE 0005                TO GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       221000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       222000-LER-CSR3-GFCTB033        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR3-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0006               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT3F-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCT3F-ERRO
                                          GFCT3F-COD-SQL-ERRO
                   MOVE 0007           TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       222000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       223000-PROCESSAR-CSR3-GFCTB033  SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 ).

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB033
                                       TO WRK-DATA-DMA.
           MOVE '/'                    TO WRK-DATA-BARRA-1.
           MOVE '/'                    TO WRK-DATA-BARRA-2.
           MOVE WRK-DATA-DMA           TO GFCT3F-DT-ENVIO-MOVTO(IND-1).
           MOVE CNRO-ARQ-MOVTO         OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1).
           MOVE CTRATO-MOVTO           OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1).
           MOVE CIDTFD-MOVTO-CRRTT     OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1).
           MOVE QEVNTO-RECBD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1).
           MOVE QEVNTO-VALDD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1).
           MOVE CSIT-MOVTO-RECBD       OF GFCTB033
                                       TO
                                       GFCT3F-SITUACAO-MOVTO-RET(IND-1).

           ADD 1                       TO GFCT3F-QTDE-TOT-REG.

           PERFORM 222000-LER-CSR3-GFCTB033.

           IF   WRK-FINALIZAR          EQUAL 'S'
                GO TO 223000-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
           ELSE
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       223000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       224000-FECHAR-CSR3-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR3-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       224000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       230000-TRATAR-CSR5-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           PERFORM 231000-ABRIR-CSR5-GFCTB033.

           PERFORM 232000-LER-CSR5-GFCTB033.

           PERFORM 233000-PROCESSAR-CSR5-GFCTB033
*************VARYING IND-1             FROM 1 BY 1
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 234000-FECHAR-CSR5-GFCTB033.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       231000-ABRIR-CSR5-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             OPEN CSR5-GFCTB033
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
               MOVE '0210'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCT3F-ERRO
               MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
               MOVE 0005                TO GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       232000-LER-CSR5-GFCTB033        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR5-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0006               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT3F-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCT3F-ERRO
                                          GFCT3F-COD-SQL-ERRO
                   MOVE 0007           TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       232000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       233000-PROCESSAR-CSR5-GFCTB033  SECTION.
      *----------------------------------------------------------------*

           PERFORM 233100-ACESSAR-GFCTB025-CANC.

           IF SQLCODE                  EQUAL +100
               ADD   1                 TO IND-1
               COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 )
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033
                                       TO WRK-DATA-DMA
               MOVE '/'                TO WRK-DATA-BARRA-1
               MOVE '/'                TO WRK-DATA-BARRA-2
               MOVE WRK-DATA-DMA       TO GFCT3F-DT-ENVIO-MOVTO(IND-1)
               MOVE CNRO-ARQ-MOVTO     OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1)
               MOVE CTRATO-MOVTO       OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1)
               MOVE CIDTFD-MOVTO-CRRTT OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1)
               MOVE QEVNTO-RECBD       OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1)
               MOVE QEVNTO-VALDD       OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1)
               MOVE CSIT-MOVTO-RECBD   OF GFCTB033
                                       TO
                                       GFCT3F-SITUACAO-MOVTO-RET(IND-1)
               ADD 1                   TO GFCT3F-QTDE-TOT-REG
           END-IF.

           PERFORM 232000-LER-CSR5-GFCTB033.


           IF   WRK-FINALIZAR          EQUAL 'S'
                GO TO 233000-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
           ELSE
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       233000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       233100-ACESSAR-GFCTB025-CANC    SECTION.
      *----------------------------------------------------------------*

           MOVE DENVIO-MOVTO-TARIF         OF GFCTB033
                                           TO DENVIO-MOVTO-CANCT
                                           OF GFCTB025.
           MOVE CROTNA-ORIGE-MOVTO         OF GFCTB033
                                           TO CROTNA-ORIGE-CANCT
                                           OF GFCTB025.
           MOVE CNRO-ARQ-MOVTO             OF GFCTB033
                                           TO CNRO-ARQ-CANCT
                                           OF GFCTB025.

           EXEC SQL
             SELECT  DENVIO_MOVTO_CANCT,
                     CROTNA_ORIGE_CANCT,
                     CNRO_ARQ_CANCT,
                     CSEQ_CANCT
             INTO   :GFCTB025.DENVIO-MOVTO-CANCT,
                    :GFCTB025.CROTNA-ORIGE-CANCT,
                    :GFCTB025.CNRO-ARQ-CANCT,
                    :GFCTB025.CSEQ-CANCT
             FROM    DB2PRD.CANCT_MOVTO_ORIGE
             WHERE   DENVIO_MOVTO_CANCT = :GFCTB025.DENVIO-MOVTO-CANCT
                 AND CROTNA_ORIGE_CANCT = :GFCTB025.CROTNA-ORIGE-CANCT
                 AND CNRO_ARQ_CANCT     = :GFCTB025.CNRO-ARQ-CANCT
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W'               )
               MOVE SPACES                 TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'             TO GFCT0M-PROGRAMA
               MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'CANCT_MOVTO_ORIGE'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'               TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
               MOVE '0230'                 TO GFCT0M-LOCAL
               MOVE SQLCA                  TO GFCT0M-SQLCA-AREA
               MOVE 9                      TO GFCT3F-ERRO
               MOVE SQLCODE                TO GFCT3F-COD-SQL-ERRO
               MOVE 0011                   TO GFCT3F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB025'
               DELIMITED BY '  '           INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       233100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       234000-FECHAR-CSR5-GFCTB033  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR5-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       234000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       240000-TRATAR-CSR1-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           PERFORM 241000-ABRIR-CSR1-GFCTB033.

           PERFORM 242000-LER-CSR1-GFCTB033.

           PERFORM 243000-PROCESSAR-CSR1-GFCTB033
             VARYING IND-1             FROM 1 BY 1
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 244000-FECHAR-CSR1-GFCTB033.

      *----------------------------------------------------------------*
       240000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       241000-ABRIR-CSR1-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             OPEN CSR1-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0250'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0005               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       241000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       242000-LER-CSR1-GFCTB033        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR1-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W'      )
               MOVE SPACES                 TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'             TO GFCT0M-PROGRAMA
               MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD'     TO GFCT0M-NOME-TAB
               MOVE 'FETCH'                TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE                TO GFCT0M-SQLCODE
               MOVE '0260'                 TO GFCT0M-LOCAL
               MOVE SQLCA                  TO GFCT0M-SQLCA-AREA
               MOVE 9                      TO GFCT3F-ERRO
               MOVE SQLCODE                TO GFCT3F-COD-SQL-ERRO
               MOVE 0006                   TO GFCT3F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE 'S'                    TO WRK-FINALIZAR
                                              GFCT3F-FIM
               IF  IND-1                   EQUAL 0
                   MOVE ZEROS              TO GFCT3F-ERRO
                                              GFCT3F-COD-SQL-ERRO
                   MOVE 0007               TO GFCT3F-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       242000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       243000-PROCESSAR-CSR1-GFCTB033  SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 ).

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB033
                                       TO WRK-DATA-DMA.
           MOVE '/'                    TO WRK-DATA-BARRA-1.
           MOVE '/'                    TO WRK-DATA-BARRA-2.
           MOVE WRK-DATA-DMA           TO GFCT3F-DT-ENVIO-MOVTO(IND-1).
           MOVE CNRO-ARQ-MOVTO         OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1).
           MOVE CTRATO-MOVTO           OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1).
           MOVE CIDTFD-MOVTO-CRRTT     OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1).
           MOVE QEVNTO-RECBD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1).
           MOVE QEVNTO-VALDD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1).

220605     IF  (CSIT-MOVTO-RECBD       OF GFCTB033
220605                                 EQUAL     1 OR 2)  AND
220605         (CNRO-ARQ-MOVTO         OF GFCTB033
220605                                 NOT EQUAL     90)
220605          PERFORM 233100-ACESSAR-GFCTB025-CANC
220605          IF SQLCODE             EQUAL     ZEROS
220605             MOVE 3              TO
220605                                 GFCT3F-SITUACAO-MOVTO-RET(IND-1)
220605          ELSE
220605             MOVE CSIT-MOVTO-RECBD         OF GFCTB033
220605                                 TO
220605                                 GFCT3F-SITUACAO-MOVTO-RET(IND-1)
220605          END-IF
220605     ELSE
220605             MOVE CSIT-MOVTO-RECBD         OF GFCTB033
220605                                 TO
220605                                 GFCT3F-SITUACAO-MOVTO-RET(IND-1)
220605     END-IF.

           ADD 1                       TO GFCT3F-QTDE-TOT-REG.

           PERFORM 242000-LER-CSR1-GFCTB033.

           IF  WRK-FINALIZAR           EQUAL 'S'
               GO TO 243000-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
           ELSE
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO   WRK-ANO
               MOVE WRK-MES-INI-DB2    TO   WRK-MES
               MOVE WRK-DIA-INI-DB2    TO   WRK-DIA
               MOVE WRK-DATA-DMA       TO   WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO   WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO   GFCT3F-PONTEIRO
                                            GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       243000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       244000-FECHAR-CSR1-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR1-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0270'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       244000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       250000-TRATAR-CSR8-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-TIPO-CONS-SITU
               WHEN 01
                   MOVE 01             TO WRK-SITU-MOVTO
               WHEN 02
                   MOVE 02             TO WRK-SITU-MOVTO
               WHEN 04
                   MOVE 04             TO WRK-SITU-MOVTO
           END-EVALUATE.

           PERFORM 251000-ABRIR-CSR8-GFCTB033.

           PERFORM 252000-LER-CSR8-GFCTB033.

220605     MOVE    +1                  TO IND-1.

           PERFORM 253000-PROCESSAR-CSR8-GFCTB033
               UNTIL IND-1             GREATER 20 OR
                                       WRK-FINALIZAR EQUAL 'S'.

           PERFORM 254000-FECHAR-CSR8-GFCTB033.

      *----------------------------------------------------------------*
       250000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       251000-ABRIR-CSR8-GFCTB033      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-SITU-MOVTO         TO CSIT-MOVTO-RECBD
                                       OF GFCTB033.

           EXEC SQL
             OPEN CSR8-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE             TO GFCT0M-SQLCODE
               MOVE '0280'              TO GFCT0M-LOCAL
               MOVE SQLCA               TO GFCT0M-SQLCA-AREA
               MOVE 9                   TO GFCT3F-ERRO
               MOVE SQLCODE             TO GFCT3F-COD-SQL-ERRO
               MOVE 0005                TO GFCT3F-COD-MSG-ERRO
                                           WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       251000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       252000-LER-CSR8-GFCTB033        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR8-GFCTB033 INTO
                   :GFCTB033.DENVIO-MOVTO-TARIF,
                   :GFCTB033.CROTNA-ORIGE-MOVTO,
                   :GFCTB033.CNRO-ARQ-MOVTO,
                   :GFCTB033.CTRATO-MOVTO,
                   :GFCTB033.CIDTFD-MOVTO-CRRTT,
                   :GFCTB033.QEVNTO-RECBD,
                   :GFCTB033.QEVNTO-VALDD,
                   :GFCTB033.CSIT-MOVTO-RECBD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0290'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0006               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCT3F-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCT3F-ERRO
                                          GFCT3F-COD-SQL-ERRO
                   MOVE 0007           TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG   TO GFCT3F-DESC-MSG-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       252000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       253000-PROCESSAR-CSR8-GFCTB033  SECTION.
      *----------------------------------------------------------------*

220605     IF   CSIT-MOVTO-RECBD       OF GFCTB033
220605                                 EQUAL     1 OR 2
220605          PERFORM 233100-ACESSAR-GFCTB025-CANC
220605          IF SQLCODE             EQUAL     ZEROS
220605             GO TO 253000-LE-CURSOR
220605          END-IF
220605     END-IF.

           COMPUTE GFCT3F-LL = ( GFCT3F-LL + 38 ).

           MOVE DENVIO-MOVTO-TARIF     OF GFCTB033
                                       TO WRK-DATA-DMA.
           MOVE '/'                    TO WRK-DATA-BARRA-1.
           MOVE '/'                    TO WRK-DATA-BARRA-2.
           MOVE WRK-DATA-DMA           TO GFCT3F-DT-ENVIO-MOVTO(IND-1).
           MOVE CNRO-ARQ-MOVTO         OF GFCTB033
                                       TO GFCT3F-CNRO-ARQ-MOVTO(IND-1).
           MOVE CTRATO-MOVTO           OF GFCTB033
                                       TO GFCT3F-CTRATO-MOVTO(IND-1).
           MOVE CIDTFD-MOVTO-CRRTT     OF GFCTB033
                                       TO
                                       GFCT3F-CIDTFD-MOVTO-CRRTT(IND-1).
           MOVE QEVNTO-RECBD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-RECBD(IND-1).
           MOVE QEVNTO-VALDD           OF GFCTB033
                                       TO GFCT3F-QEVNTO-VALDD(IND-1).
           MOVE CSIT-MOVTO-RECBD       OF GFCTB033
                                       TO
                                       GFCT3F-SITUACAO-MOVTO-RET(IND-1).

           ADD 1                       TO GFCT3F-QTDE-TOT-REG.

220605     ADD +1                      TO IND-1.

220605 253000-LE-CURSOR.

           PERFORM 252000-LER-CSR8-GFCTB033.

           IF   WRK-FINALIZAR          EQUAL 'S'
                GO TO 253000-99-FIM
           END-IF.

           IF  IND-1                   GREATER 20
               MOVE 'S'                TO WRK-FINALIZAR
               IF  WRK-PONTEIRO        EQUAL SPACES
                   MOVE WRK-FILTRO     TO WRK-PONTEIRO
               END-IF
           ELSE
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(1:2)
                                       TO WRK-DIA-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(4:2)
                                       TO WRK-MES-INI-DB2
               MOVE DENVIO-MOVTO-TARIF OF GFCTB033(7:4)
                                       TO WRK-ANO-INI-DB2
               MOVE WRK-ANO-INI-DB2    TO WRK-ANO
               MOVE WRK-MES-INI-DB2    TO WRK-MES
               MOVE WRK-DIA-INI-DB2    TO WRK-DIA
               MOVE WRK-DATA-DMA       TO WRK-PONTEI-DENV-INI
               MOVE CNRO-ARQ-MOVTO     TO WRK-PONTEI-CSEQ-MOVTO
               MOVE WRK-PONTEIRO       TO GFCT3F-PONTEIRO
                                          GFCT3E-PONTEIRO
           END-IF.

      *----------------------------------------------------------------*
       253000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       254000-FECHAR-CSR8-GFCTB033     SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR8-GFCTB033
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)  OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0354'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'CTRL_MOVTO_RECBD' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0300'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT3F-ERRO
               MOVE SQLCODE            TO GFCT3F-COD-SQL-ERRO
               MOVE 0011               TO GFCT3F-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
                      ' - GFCTB033'
               DELIMITED BY '  '       INTO GFCT3F-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       254000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
