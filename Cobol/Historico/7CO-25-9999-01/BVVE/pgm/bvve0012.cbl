      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE0012.
       AUTHOR.     FRANCISMARA PAGNONCELLI.
      *                                                                *
      *================================================================*
      *                  B S I  T E C N O L O G I A                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....: BVVE0012                                      *
      *    PROGRAMADORA: FRANCISMARA PAGNONCELLI  - BSI                *
      *    ANALISTA....: FRANCISMARA PAGNONCELLI  - BSI                *
      *    DATA........: JULHO/2012                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: CONSOLIDAR MOVIMENTO DO DIA COM O ARQUIVO DE  *
      *    VARREDURA RECEBIDO DO TMNF                                  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#SBATMV - AREA PARA CHAMADA DA SBAT3510                    *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT DADREL ASSIGN        TO UT-S-DADREL
                    FILE STATUS        IS WRK-FS-DADREL.

           SELECT RELATO   ASSIGN      TO UT-S-RELATO
                    FILE STATUS        IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO DE VARREDURA CIP                            *
      *            ORG. FIXO - LRECL = 2925                            *
      *----------------------------------------------------------------*

       FD  DADREL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-DADREL            PIC  X(159).


      *----------------------------------------------------------------*
      *    OUTPUT- RELATORIO DE INCONSISTENCIAS                        *
      *            ORG. FIXO - LRECL = 133                             *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELATO              PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVE0012 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-DADREL          PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-LINHAS              PIC  9(09) COMP-3   VALUE 99.
           03  ACU-PAGINAS             PIC  9(05) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *----------------------------------------------------------------*
       77  WRK-FS-DADREL               PIC X(02)           VALUE SPACES.
       77  WRK-FS-RELATO               PIC X(02)           VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)  VALUE ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC X(13)  VALUE ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC X(13)  VALUE 'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC X(13)  VALUE ' NA LEITURA  '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)      VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(008)     VALUE 'BATCH'.
       01  WRK-MASCARA                 PIC  ZZZ.ZZ9.999 VALUE ZEROS.

       01  WRK-DATA-AAAAMMDD.
           03  WRK-ANO                 PIC  9(004)     VALUE ZEROS.
           03  WRK-MES                 PIC  9(002)     VALUE ZEROS.
           03  WRK-DIA                 PIC  9(002)     VALUE ZEROS.
       01  WRK-DATA-AAAAMMDD-R REDEFINES WRK-DATA-AAAAMMDD
                                       PIC  9(008).

       01  WRK-DATA-ANT                PIC  9(008)     VALUE ZEROS.

       01  WRK-CHV-DADREL.
           03  WRK-CHV-CNPJ            PIC  9(008)     VALUE ZEROS.

       01  WRK-CHV-DADREL-ANT          PIC  9(008)     VALUE ZEROS.


       01  WRK-FORMAT-CGCCPF.
7C2511*    03  WRK-NROCNPJ             PIC  9(008)       VALUE ZEROS.
7C2511     03  WRK-NROCNPJ             PIC  X(008)       VALUE SPACES.
           03  WRK-BARRA               PIC  X(001)       VALUE SPACES.
7C2511*    03  WRK-FILIAL              PIC  9(004)       VALUE ZEROS.
7C2511     03  WRK-FILIAL              PIC  X(004)       VALUE SPACES.
           03  WRK-TRACO               PIC  X(001)       VALUE SPACES.
           03  WRK-CTRL-CNPJ           PIC  9(002)       VALUE ZEROS.
       01  WRK-FORNAT-CPF REDEFINES WRK-FORMAT-CGCCPF.
7C2511*    03  WRK-CPF                 PIC  9(009).
7C2511     03  WRK-CPF                 PIC  X(009)       VALUE SPACES.
           03  WRK-TRACO-2             PIC  X(001).
           03  WRK-CTRL-CPF            PIC  9(002).

       01  WRK-CGCCPF.
7C2511*    03  WRK-NROCNPJ             PIC  9(008)       VALUE ZEROS.
7C2511     03  WRK-NROCNPJ             PIC  X(008)       VALUE SPACES.
7C2511*    03  WRK-FILIAL              PIC  9(004)       VALUE ZEROS.
7C2511     03  WRK-FILIAL              PIC  X(004)       VALUE SPACES.
           03  WRK-CTRL-CNPJ           PIC  9(002)       VALUE ZEROS.
       01  WRK-SO-CPF REDEFINES WRK-CGCCPF.
           03  FILLER                  PIC  9(003).
7C2511*    03  WRK-CPF                 PIC  9(009).
7C2511     03  WRK-CPF                 PIC  X(009)       VALUE SPACES.
           03  WRK-CTRL-CPF            PIC  9(002).
       01  WRK-CGCCPF-N REDEFINES WRK-SO-CPF
                                       PIC  9(014).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA MENSAGENS DE ERRO     *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERRO.
          03  FILLER                   PIC X(07)   VALUE SPACES.
          03  FILLER                   PIC X(07)   VALUE
              '* ERRO '.
          03  WRK-OPERACAO             PIC X(13)   VALUE SPACES.
          03  FILLER                   PIC X(12)   VALUE
              ' DO ARQUIVO '.
          03  WRK-NOME-ARQ             PIC X(08)   VALUE SPACES.
          03  FILLER                   PIC X(17)   VALUE
              ' - FILE-STATUS = '.
          03  WRK-FILE-STATUS          PIC X(02)   VALUE SPACES.
          03  FILLER                   PIC X(02)   VALUE ' *'.

      *----------------------------------------------------------------*
      *    '*** AREA DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  FILLER                  PIC  9(005) COMP-3  VALUE ZEROS.
           05  FILLER                  PIC  9(007) COMP-3  VALUE ZEROS.
           05  FILLER                  PIC  9(009) COMP-3  VALUE ZEROS.
           05  FILLER                  PIC  9(007) COMP-3  VALUE ZEROS.
           05  FILLER                  PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TIMES.
             10  WRK-ANO-TIMES         PIC  9(004)         VALUE ZEROS.
             10  WRK-MES-TIMES         PIC  9(002)         VALUE ZEROS.
             10  WRK-DIA-TIMES         PIC  9(002)         VALUE ZEROS.
             10  WRK-HORA-TIMES        PIC  9(002)         VALUE ZEROS.
             10  WRK-HORA-MIN          PIC  9(002)         VALUE ZEROS.
             10  WRK-HORA-SEG          PIC  9(002)         VALUE ZEROS.
             10  FILLER                PIC  X(006)         VALUE SPACES.

         05  WRK-TIMESTAMP.
           10 WRK-TIMESTAMP-AAAA       PIC  9(04)   VALUE ZEROS.
           10 WRK-TIMESTAMP-MM         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-DD         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-HH         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-MN         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-SS         PIC  9(02)   VALUE ZEROS.
           10 WRK-TIMESTAMP-MIL        PIC  9(06)   VALUE ZEROS.

         05  WRK-TIMESTAMP-EDT.
           10 WRK-TIMESTAMP-AAAA       PIC  9(04)   VALUE ZEROS.
           10 FILLER                   PIC  X(01)   VALUE '-'.
           10 WRK-TIMESTAMP-MM         PIC  9(02)   VALUE ZEROS.
           10 FILLER                   PIC  X(01)   VALUE '-'.
           10 WRK-TIMESTAMP-DD         PIC  9(02)   VALUE ZEROS.
           10 FILLER                   PIC  X(01)   VALUE '-'.
           10 WRK-TIMESTAMP-HH         PIC  9(02)   VALUE ZEROS.
           10 FILLER                   PIC  X(01)   VALUE ':'.
           10 WRK-TIMESTAMP-MN         PIC  9(02)   VALUE ZEROS.
           10 FILLER                   PIC  X(01)   VALUE ':'.
           10 WRK-TIMESTAMP-SS         PIC  9(02)   VALUE ZEROS.
           10 FILLER                   PIC  X(01)   VALUE ':'.
           10 WRK-TIMESTAMP-MIL        PIC  9(06)   VALUE ZEROS.


       01  WRK-DATA-PROC.
           03 WRK-DIA                  PIC  9(02)         VALUE ZEROS.
           03 WRK-PROC-TRACO1          PIC  X(01)         VALUE  '/'.
           03 WRK-MES                  PIC  9(02)         VALUE ZEROS.
           03 WRK-PROC-TRACO2          PIC  X(01)         VALUE  '/'.
           03 WRK-ANO                  PIC  9(04)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)         VALUE
           '* AREA PARA RELATORIO'.
      *----------------------------------------------------------------*

       01  WRK-CABEC-1.
           05  FILLER                   PIC  X(001)        VALUE '1'.
           05  WRK-CABEC1-DIA           PIC  9(002)        VALUE ZEROS.
           05  FILLER                   PIC  X(001)        VALUE '/'.
           05  WRK-CABEC1-MES           PIC  9(002)        VALUE ZEROS.
           05  FILLER                   PIC  X(001)        VALUE '/'.
           05  WRK-CABEC1-ANO           PIC  9(004)        VALUE ZEROS.
           05  FILLER                   PIC  X(039)        VALUE SPACES.
           05  WRK-CABEC1-TITULO        PIC  X(038)        VALUE
               'MANUTENCAO DE DOMICILIO BANCARIO - CIP'.
           05  FILLER                   PIC  X(037)        VALUE SPACES.
           05  WRK-CABEC1-HORA          PIC  9(002)        VALUE ZEROS.
           05  FILLER                   PIC  X(001)        VALUE ':'.
           05  WRK-CABEC1-MIN           PIC  9(002)        VALUE ZEROS.
           05  FILLER                   PIC  X(001)        VALUE ':'.
           05  WRK-CABEC1-SEG           PIC  9(002)        VALUE ZEROS.

       01  WRK-CABEC-2.
           05  FILLER                   PIC  X(001)        VALUE ' '.
           05  FILLER                   PIC  X(008)        VALUE
               'BVVE0012'.
           05  FILLER                   PIC  X(023)        VALUE SPACES.
           05  FILLER                   PIC  X(070)        VALUE
               'RELACAO DE MANUTENCOES RECEBIDAS NO ARQUIVO DE VARREDURA
      -        ' - ASCG0009'.
           05  FILLER                   PIC  X(022)        VALUE SPACES.
           05  FILLER                   PIC  X(004)       VALUE 'PAG.'.
           05  WRK-CABEC2-PAG           PIC  9(005)        VALUE ZEROS.

       01  WRK-CABEC-3.
           05  FILLER                   PIC  X(001)        VALUE '-'.
           05  FILLER                   PIC  X(019)        VALUE
               'DATA DO MOVIMENTO: '.
           05  WRK-CABEC3-DATA          PIC  X(010)        VALUE SPACES.

       01  WRK-CABEC-4.
           05  FILLER                   PIC  X(001)        VALUE '-'.
           05  FILLER                   PIC  X(132)        VALUE
               'INCLUSAO NO HISTORICO: FILIAIS RETORNADAS NA INCLUSAO DE
      -        ' MANUTENCAO POR CNPJ RAIZ'.

       01  WRK-CABEC-5.
           05  FILLER                   PIC  X(001)        VALUE '0'.
           05  FILLER                   PIC  X(132)        VALUE
               'CNPJ RAIZ        NUMERO SCG  BANDEIRA   DATA INICIAL DAT
      -        'A FINAL  FILIAIS INCLUIDAS'.

       01  WRK-CABEC-6.
           05  FILLER                   PIC  X(001)        VALUE '-'.
           05  FILLER                   PIC  X(132)        VALUE
           'INCLUSAO NO HISTORICO: MANUTENCOES NAO EFETUADAS PELO BVVE R
      -    'ECEBIDAS NA VARREDURA'.


       01  WRK-CABEC-7.
           05  FILLER                   PIC  X(001)        VALUE '0'.
           05  FILLER                   PIC  X(132)        VALUE
           'CPF/CNPJ                   NUMERO SCG  TP MANUT.           B
      -    'ANDEIRA   IDENT.CRED.  DATA FINAL  DATA HORA MANUTENCAO'.

       01  WRK-CABEC-8.
           05  FILLER                   PIC  X(001)        VALUE '0'.
           05  FILLER                   PIC  X(132)        VALUE
              'INCONSISTENCIA: MANUTENCOES PARA CPNJ/CPF ENVIADAS E NAO
      -       'RECEBIDAS NA VARREDURA'.

       01  WRK-CABEC-9.
           05  FILLER                   PIC  X(001)        VALUE '0'.
           05  FILLER                   PIC  X(132)        VALUE
           'CPF/CNPJ                   NUMERO SCG  TP MANUT.   BANDEIRA
      -    '   DATA FINAL   DATA HORA MANUTENCAO'.

       01  WRK-CABEC-10.
           05  FILLER                   PIC  X(001)        VALUE '0'.
           05  FILLER                   PIC  X(132)        VALUE
           'INCONSISTENCIA: TIPO DE MANUTENCAO PARA O CNPJ/CPF NAO CORRE
      -    'SPONDE AO ENVIADO PELO BVVE'.

       01  WRK-CABEC-11.
           05  FILLER                   PIC  X(001)        VALUE '0'.
           05  FILLER                   PIC  X(132)        VALUE
           'CPF/CNPJ                      NUM SCG  MANUT ENV.   BANDEIRA
      -    '   DATA/HORA ENVIO     MANUT REC.  BANDEIRA    DATA/HORA REC
      -    ' '.

       01  WRK-LINDET-1.
           03  WRK-LD1-CARRO           PIC  X(001)        VALUE '0'.
           03  WRK-LD1-PARTE-1.
               05  WRK-LD1-RAIZ        PIC  9(008)        VALUE ZEROS.
               05  FILLER              PIC  X(002)        VALUE SPACES.
               05  WRK-LD1-NUM-SCG     PIC  9(017)        VALUE ZEROS.
               05  FILLER              PIC  X(002)        VALUE SPACES.
               05  WRK-LD1-BAND        PIC  X(010)        VALUE SPACES.
               05  FILLER              PIC  X(002)        VALUE SPACES.
               05  WRK-LD1-DT-INI      PIC  X(010)        VALUE SPACES.
               05  FILLER              PIC  X(002)        VALUE SPACES.
               05  WRK-LD1-DT-FINAL    PIC  X(010)        VALUE SPACES.
           03  FILLER                  PIC  X(003)        VALUE SPACES.
           03  WRK-LD1-FILIAL          PIC  X(018)        VALUE SPACES.

       01  WRK-LINDET-2.
           05  WRK-LD2-CARRO           PIC  X(001)        VALUE ' '.
           05  WRK-LD2-CNPJ            PIC  X(018)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD2-NUM-SCG         PIC  9(017)        VALUE ZEROS.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD2-TP-MAN          PIC  X(019)        VALUE SPACES.
           05  FILLER                  PIC  X(001)        VALUE SPACES.
           05  WRK-LD2-BAND            PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(004)        VALUE SPACES.
           05  WRK-LD2-IDENT           PIC  X(008)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD2-DT-FINAL        PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD2-DTHR-MANUT      PIC  X(020)        VALUE SPACES.

       01  WRK-LINDET-3.
           05  WRK-LD3-CARRO           PIC  X(001)        VALUE ' '.
           05  WRK-LD3-CNPJ            PIC  X(018)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD3-NUM-SCG         PIC  9(017)        VALUE ZEROS.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD3-TP-MAN          PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD3-BAND            PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD3-DT-FINAL        PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(003)        VALUE SPACES.
           05  WRK-LD3-DTHR-MANUT      PIC  X(020)        VALUE SPACES.

       01  WRK-LINDET-4.
           05  WRK-LD4-CARRO           PIC  X(001)        VALUE ' '.
           05  WRK-LD4-CNPJ            PIC  X(018)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD4-NUM-SCG         PIC  9(017)        VALUE ZEROS.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD4-MAN-ENV         PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(003)        VALUE SPACES.
           05  WRK-LD4-BAND-ENV        PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(001)        VALUE SPACES.
           05  WRK-LD4-DTHR-ENV        PIC  X(020)        VALUE SPACES.
           05  FILLER                  PIC  X(001)        VALUE SPACES.
           05  WRK-LD4-MAN-REC         PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD4-BAND-REC        PIC  X(010)        VALUE SPACES.
           05  FILLER                  PIC  X(002)        VALUE SPACES.
           05  WRK-LD4-DTHR-REC        PIC  X(020)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA ARQUIVO DADREL'.
      *----------------------------------------------------------------*

       01  WRK-DADREL.
           03 WRK-TP-REGISTRO                  PIC X(001)  VALUE SPACES.
           03 WRK-DATA-MOV                     PIC 9(008)  VALUE ZEROS.
           03 WRK-AREA-DADOS                   PIC X(150)  VALUE SPACES.
           03 WRK-REG-TIPO-1 REDEFINES WRK-AREA-DADOS.
              05 WRK-RG1-CNPJ-CPF-CLI          PIC 9(014).
              05 WRK-RG1-NUM-IDENTC-SCG        PIC 9(017).
              05 WRK-RG1-COD-BANDEIRA          PIC X(003).
              05 WRK-RG1-DTINI-VALIDD          PIC 9(008).
              05 WRK-RG1-DTFIM-VALIDD          PIC 9(008).
              05 FILLER                        PIC X(100).
           03 WRK-REG-TIPO-2 REDEFINES WRK-AREA-DADOS.
              05 WRK-RG2-CNPJ-CPF-CLI          PIC 9(014).
              05 WRK-RG2-NUM-IDENTC-SCG        PIC 9(017).
              05 WRK-RG2-TP-PSSOA              PIC X(001).
              05 WRK-RG2-COD-BANDEIRA          PIC X(003).
              05 WRK-RG2-TP-BAIXA-DOMCL        PIC X(001).
              05 WRK-RG2-TP-MANUT-DOMCL        PIC X(001).
              05 WRK-RG2-NOM-CREDDR            PIC X(008).
              05 WRK-RG2-DTHR-MANUT            PIC X(020).
              05 WRK-RG2-DTFIM-VALIDD          PIC 9(008).
              05 FILLER                        PIC X(077).
           03 WRK-REG-TIPO-3 REDEFINES WRK-AREA-DADOS.
              05 WRK-RG3-CNPJ-CPF-CLI          PIC 9(014).
              05 WRK-RG3-NUM-IDENTC-SCG        PIC 9(017).
              05 WRK-RG3-TP-PSSOA              PIC X(001).
              05 WRK-RG3-COD-BANDEIRA          PIC X(003).
              05 WRK-RG3-TP-MANUT-DOMCL        PIC X(001).
              05 WRK-RG3-DT-FINAL              PIC 9(008).
              05 WRK-RG3-DTHR-MANUT            PIC X(020).
              05 FILLER                        PIC X(086).
           03 WRK-REG-TIPO-4 REDEFINES WRK-AREA-DADOS.
              05 WRK-RG4-CNPJ-CPF-CLI          PIC 9(014).
              05 WRK-RG4-NUM-IDENTC-SCG        PIC 9(017).
              05 WRK-RG4-TP-PSSOA              PIC X(001).
              05 WRK-RG4-COD-BAND-ENV          PIC X(003).
              05 WRK-RG4-TP-MANUT-ENV          PIC X(001).
              05 WRK-RG4-DTHR-ENV              PIC X(020).
              05 WRK-RG4-COD-BAND-REC          PIC X(003).
              05 WRK-RG4-TP-MANUT-REC          PIC X(001).
              05 WRK-RG4-DTHR-REC              PIC X(020).
              05 FILLER                        PIC X(070).


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA DA POOL7100  *'.
      *----------------------------------------------------------------*
           COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING BVVE0012    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-TESTAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-CHV-DADREL      EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

           STOP RUN.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT  DADREL
               OUTPUT  RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           PERFORM 1400-OBTER-DATA-CORRENTE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-DADREL.

           PERFORM 1240-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-TESTAR-FS-DADREL           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DADREL           NOT EQUAL '00'
               MOVE 'DADREL'           TO WRK-NOME-ARQ
               MOVE WRK-FS-DADREL      TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1240-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE 'RELATO'           TO WRK-NOME-ARQ
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-OBTER-DATA-CORRENTE        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-DATA-HORA.

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE  WRK-ANO-TIMES         TO WRK-CABEC1-ANO
           MOVE  WRK-MES-TIMES         TO WRK-CABEC1-MES
           MOVE  WRK-DIA-TIMES         TO WRK-CABEC1-DIA.
           MOVE  WRK-HORA-TIMES        TO WRK-CABEC1-HORA.
           MOVE  WRK-HORA-MIN          TO WRK-CABEC1-MIN.
           MOVE  WRK-HORA-SEG          TO WRK-CABEC1-SEG.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-TESTAR-VAZIO               SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-DADREL.

           IF  WRK-FS-DADREL   NOT   EQUAL '00'
               DISPLAY   '***********BVVE0012******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ    DADREL   VAZIO      * '
               DISPLAY   '*                                    * '
               DISPLAY   '************************************** '
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-DADREL                 SECTION.
      *----------------------------------------------------------------*

           READ DADREL                 INTO WRK-DADREL.

           IF  WRK-FS-DADREL           EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHV-DADREL
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-DADREL.

           ADD     1                   TO ACU-LID-DADREL.

           MOVE  WRK-RG1-CNPJ-CPF-CLI(1:8) TO WRK-CHV-CNPJ.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-DATA-MOV            TO  WRK-DATA-AAAAMMDD-R
                                             WRK-DATA-ANT.
           MOVE  CORR WRK-DATA-AAAAMMDD  TO  WRK-DATA-PROC
           MOVE  WRK-DATA-PROC           TO  WRK-CABEC3-DATA.

           PERFORM 3100-IMPRIMIR-TIPO-1
                   UNTIL WRK-TP-REGISTRO   NOT EQUAL '1' OR
                   WRK-DATA-MOV            NOT EQUAL WRK-DATA-ANT OR
                   WRK-CHV-DADREL          EQUAL HIGH-VALUES.

           MOVE 99                    TO   ACU-LINHAS.

           PERFORM 3200-IMPRIMIR-TIPO-2
                   UNTIL WRK-TP-REGISTRO   NOT EQUAL '2' OR
                   WRK-DATA-MOV            NOT EQUAL WRK-DATA-ANT OR
                   WRK-CHV-DADREL          EQUAL HIGH-VALUES.

           MOVE 99                    TO   ACU-LINHAS.

           PERFORM 3300-IMPRIMIR-TIPO-3
                   UNTIL WRK-TP-REGISTRO   NOT EQUAL '3' OR
                   WRK-DATA-MOV            NOT EQUAL WRK-DATA-ANT OR
                   WRK-CHV-DADREL          EQUAL HIGH-VALUES.

           MOVE 99                    TO   ACU-LINHAS.

           PERFORM 3400-IMPRIMIR-TIPO-4
                   UNTIL WRK-TP-REGISTRO   NOT EQUAL '4' OR
                   WRK-DATA-MOV            NOT EQUAL WRK-DATA-ANT OR
                   WRK-CHV-DADREL          EQUAL HIGH-VALUES.

           MOVE 99                    TO   ACU-LINHAS.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-IMPRIMIR-TIPO-1           SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-RG1-CNPJ-CPF-CLI(1:8) TO  WRK-LD1-RAIZ
           MOVE  WRK-RG1-NUM-IDENTC-SCG    TO  WRK-LD1-NUM-SCG

           MOVE  WRK-RG1-DTINI-VALIDD   TO WRK-DATA-AAAAMMDD-R
           MOVE  CORR WRK-DATA-AAAAMMDD TO WRK-DATA-PROC
           MOVE  WRK-DATA-PROC          TO WRK-LD1-DT-INI

           MOVE  WRK-RG1-DTFIM-VALIDD   TO WRK-DATA-AAAAMMDD-R
           MOVE  CORR WRK-DATA-AAAAMMDD TO WRK-DATA-PROC
           MOVE  WRK-DATA-PROC          TO WRK-LD1-DT-FINAL

           MOVE WRK-CHV-CNPJ            TO WRK-CHV-DADREL-ANT.

           PERFORM 3120-EMITIR-DETALHES
                   UNTIL  WRK-CHV-CNPJ NOT EQUAL WRK-CHV-DADREL-ANT.

      *----------------------------------------------------------------*
       3100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110-IMPRIMIR-CABEC             SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINAS.
           MOVE ACU-PAGINAS            TO WRK-CABEC2-PAG.

           WRITE FD-REG-RELATO         FROM WRK-CABEC-1.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1240-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM WRK-CABEC-2.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1240-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM WRK-CABEC-3.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           PERFORM 1240-TESTAR-FS-RELATO.

           MOVE 05                     TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3120-EMITIR-DETALHES            SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3110-IMPRIMIR-CABEC
               WRITE FD-REG-RELATO     FROM WRK-CABEC-4
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               WRITE FD-REG-RELATO     FROM WRK-CABEC-5
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               ADD  05                 TO ACU-LINHAS
               MOVE '0'                TO WRK-LD1-CARRO
           END-IF.

           MOVE  WRK-RG1-CNPJ-CPF-CLI  TO WRK-CGCCPF-N
           MOVE  CORR WRK-CGCCPF       TO WRK-FORMAT-CGCCPF
           MOVE  '/'                   TO WRK-BARRA
           MOVE  '-'                   TO WRK-TRACO
           MOVE  WRK-FORMAT-CGCCPF     TO WRK-LD1-FILIAL

           IF  WRK-RG1-COD-BANDEIRA    EQUAL '003'
               MOVE 'MASTERCARD'       TO WRK-LD1-BAND
           ELSE
               MOVE 'VISA'             TO WRK-LD1-BAND
           END-IF.

           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO
           WRITE FD-REG-RELATO         FROM WRK-LINDET-1
           PERFORM 1240-TESTAR-FS-RELATO
           ADD   1                     TO ACU-LINHAS

           MOVE  SPACES                TO WRK-LD1-PARTE-1
                                          WRK-LD1-CARRO.

           PERFORM 2100-LER-DADREL.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-IMPRIMIR-TIPO-2           SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3110-IMPRIMIR-CABEC
               WRITE FD-REG-RELATO     FROM WRK-CABEC-6
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               WRITE FD-REG-RELATO     FROM WRK-CABEC-7
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               ADD  05                 TO ACU-LINHAS
               MOVE '0'                TO WRK-LD2-CARRO
           END-IF.

           MOVE  WRK-RG2-CNPJ-CPF-CLI  TO  WRK-CGCCPF-N

           EVALUATE  WRK-RG2-TP-PSSOA
               WHEN 'R'
                   MOVE WRK-CGCCPF(1:8)   TO  WRK-LD2-CNPJ
               WHEN 'J'
                   MOVE CORR WRK-CGCCPF   TO  WRK-FORMAT-CGCCPF
                   MOVE '/'               TO WRK-BARRA
                   MOVE '-'               TO WRK-TRACO
                   MOVE WRK-FORMAT-CGCCPF TO WRK-LD2-CNPJ
               WHEN 'F'
                   MOVE CORR WRK-SO-CPF   TO WRK-FORNAT-CPF
                   MOVE '-'               TO WRK-TRACO-2
                   MOVE WRK-FORNAT-CPF    TO WRK-LD2-CNPJ
           END-EVALUATE.

           MOVE  WRK-RG2-NUM-IDENTC-SCG    TO  WRK-LD2-NUM-SCG

           IF  WRK-RG2-COD-BANDEIRA        EQUAL '003'
               MOVE 'MASTERCARD'           TO WRK-LD2-BAND
           ELSE
               MOVE 'VISA'                 TO WRK-LD2-BAND
           END-IF.

           MOVE  WRK-RG2-DTFIM-VALIDD    TO WRK-DATA-AAAAMMDD-R
           MOVE  CORR WRK-DATA-AAAAMMDD  TO WRK-DATA-PROC
           MOVE  WRK-DATA-PROC           TO WRK-LD2-DT-FINAL

           MOVE  WRK-RG2-NOM-CREDDR    TO WRK-LD2-IDENT
           MOVE  WRK-RG2-DTHR-MANUT    TO WRK-LD2-DTHR-MANUT

           EVALUATE TRUE
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'I'
                   MOVE 'INCLUSAO'          TO  WRK-LD2-TP-MAN
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'A'
                   MOVE 'ALTERACAO'         TO  WRK-LD2-TP-MAN
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'C'
                   MOVE 'INCLUSAO CREDENC.' TO  WRK-LD2-TP-MAN
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'B' AND
                    WRK-RG2-TP-BAIXA-DOMCL  EQUAL '1'
                   MOVE 'BAIXA SOL. BANCO ' TO  WRK-LD2-TP-MAN
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'B' AND
                    WRK-RG2-TP-BAIXA-DOMCL  EQUAL '2'
                   MOVE 'BAIXA ERRO REGISTRO' TO  WRK-LD2-TP-MAN
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'B' AND
                    WRK-RG2-TP-BAIXA-DOMCL  EQUAL '3'
                   MOVE 'BAIXA SUBSTITUICAO ' TO  WRK-LD2-TP-MAN
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'B' AND
                    WRK-RG2-TP-BAIXA-DOMCL  EQUAL '4'
                   MOVE 'BAIXA DECURSO PRAZO' TO  WRK-LD2-TP-MAN
               WHEN WRK-RG2-TP-MANUT-DOMCL  EQUAL 'B' AND
                    WRK-RG2-TP-BAIXA-DOMCL  EQUAL '5'
                   MOVE 'BAIXA FALTA COMPROV' TO  WRK-LD2-TP-MAN
           END-EVALUATE.

           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO
           WRITE FD-REG-RELATO         FROM WRK-LINDET-2
           PERFORM 1240-TESTAR-FS-RELATO
           ADD   1                     TO ACU-LINHAS.

           PERFORM 2100-LER-DADREL.

      *----------------------------------------------------------------*
       3200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-IMPRIMIR-TIPO-3           SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3110-IMPRIMIR-CABEC
               WRITE FD-REG-RELATO     FROM WRK-CABEC-8
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               WRITE FD-REG-RELATO     FROM WRK-CABEC-9
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               ADD  05                 TO ACU-LINHAS
               MOVE '0'                TO WRK-LD2-CARRO
           END-IF.

           MOVE  WRK-RG3-CNPJ-CPF-CLI  TO  WRK-CGCCPF-N

           EVALUATE  WRK-RG3-TP-PSSOA
               WHEN 'R'
                   MOVE WRK-CGCCPF(1:8)   TO  WRK-LD3-CNPJ
               WHEN 'J'
                   MOVE CORR WRK-CGCCPF   TO  WRK-FORMAT-CGCCPF
                   MOVE '/'               TO WRK-BARRA
                   MOVE '-'               TO WRK-TRACO
                   MOVE WRK-FORMAT-CGCCPF TO WRK-LD3-CNPJ
               WHEN 'F'
                   MOVE CORR WRK-SO-CPF   TO WRK-FORNAT-CPF
                   MOVE '-'               TO WRK-TRACO-2
                   MOVE WRK-FORNAT-CPF    TO WRK-LD3-CNPJ
           END-EVALUATE.

           MOVE  WRK-RG3-NUM-IDENTC-SCG    TO  WRK-LD3-NUM-SCG

           IF  WRK-RG3-COD-BANDEIRA        EQUAL '003'
               MOVE 'MASTERCARD'           TO WRK-LD3-BAND
           ELSE
               MOVE 'VISA'                 TO WRK-LD3-BAND
           END-IF.

           MOVE  WRK-RG3-DT-FINAL       TO WRK-DATA-AAAAMMDD-R
           MOVE  CORR WRK-DATA-AAAAMMDD TO WRK-DATA-PROC
           MOVE  WRK-DATA-PROC          TO WRK-LD3-DT-FINAL

           MOVE  WRK-RG3-DTHR-MANUT    TO WRK-LD3-DTHR-MANUT

           EVALUATE TRUE
               WHEN WRK-RG3-TP-MANUT-DOMCL  EQUAL 'I'
                   MOVE 'INCLUSAO'          TO  WRK-LD3-TP-MAN
               WHEN WRK-RG3-TP-MANUT-DOMCL  EQUAL 'A'
                   MOVE 'ALTERACAO'         TO  WRK-LD3-TP-MAN
               WHEN WRK-RG3-TP-MANUT-DOMCL  EQUAL 'B'
                   MOVE 'BAIXA    '         TO  WRK-LD3-TP-MAN
           END-EVALUATE.


           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO
           WRITE FD-REG-RELATO         FROM WRK-LINDET-3
           PERFORM 1240-TESTAR-FS-RELATO
           ADD   1                     TO ACU-LINHAS.

           PERFORM 2100-LER-DADREL.

      *----------------------------------------------------------------*
       3300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-IMPRIMIR-TIPO-4           SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3110-IMPRIMIR-CABEC
               WRITE FD-REG-RELATO     FROM WRK-CABEC-10
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               WRITE FD-REG-RELATO     FROM WRK-CABEC-11
               MOVE WRK-GRAVACAO       TO WRK-OPERACAO
               PERFORM 1240-TESTAR-FS-RELATO
               ADD  05                 TO ACU-LINHAS
               MOVE '0'                TO WRK-LD2-CARRO
           END-IF.

           MOVE  WRK-RG4-CNPJ-CPF-CLI  TO  WRK-CGCCPF-N

           EVALUATE  WRK-RG4-TP-PSSOA
               WHEN 'R'
                   MOVE WRK-CGCCPF(1:8)   TO  WRK-LD4-CNPJ
               WHEN 'J'
                   MOVE CORR WRK-CGCCPF   TO  WRK-FORMAT-CGCCPF
                   MOVE '/'               TO WRK-BARRA
                   MOVE '-'               TO WRK-TRACO
                   MOVE WRK-FORMAT-CGCCPF TO WRK-LD4-CNPJ
               WHEN 'F'
                   MOVE CORR WRK-SO-CPF   TO WRK-FORNAT-CPF
                   MOVE '-'               TO WRK-TRACO-2
                   MOVE WRK-FORNAT-CPF    TO WRK-LD4-CNPJ
           END-EVALUATE.

           MOVE  WRK-RG4-NUM-IDENTC-SCG    TO  WRK-LD4-NUM-SCG

           IF  WRK-RG4-COD-BAND-ENV        EQUAL '003'
               MOVE 'MASTERCARD'           TO WRK-LD4-BAND-ENV
           ELSE
               MOVE 'VISA'                 TO WRK-LD4-BAND-ENV
           END-IF.

           IF  WRK-RG4-COD-BAND-REC        EQUAL '003'
               MOVE 'MASTERCARD'           TO WRK-LD4-BAND-REC
           ELSE
               MOVE 'VISA'                 TO WRK-LD4-BAND-REC
           END-IF.

           MOVE  WRK-RG4-DTHR-ENV          TO WRK-LD4-DTHR-ENV
           MOVE  WRK-RG4-DTHR-REC          TO WRK-LD4-DTHR-REC

           EVALUATE TRUE
               WHEN WRK-RG4-TP-MANUT-ENV  EQUAL 'I'
                   MOVE 'INCLUSAO'        TO  WRK-LD4-MAN-ENV
               WHEN WRK-RG4-TP-MANUT-ENV  EQUAL 'A'
                   MOVE 'ALTERACAO'       TO  WRK-LD4-MAN-ENV
               WHEN WRK-RG4-TP-MANUT-ENV  EQUAL 'B'
                   MOVE 'BAIXA    '       TO  WRK-LD4-MAN-ENV
           END-EVALUATE.

           EVALUATE TRUE
               WHEN WRK-RG4-TP-MANUT-REC  EQUAL 'I'
                   MOVE 'INCLUSAO'        TO  WRK-LD4-MAN-REC
               WHEN WRK-RG4-TP-MANUT-REC  EQUAL 'A'
                   MOVE 'ALTERACAO'       TO  WRK-LD4-MAN-REC
               WHEN WRK-RG4-TP-MANUT-REC  EQUAL 'B'
                   MOVE 'BAIXA    '       TO  WRK-LD4-MAN-REC
           END-EVALUATE.

           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO
           WRITE FD-REG-RELATO         FROM WRK-LINDET-4
           PERFORM 1240-TESTAR-FS-RELATO
           ADD   1                     TO ACU-LINHAS.

           PERFORM 2100-LER-DADREL.

      *----------------------------------------------------------------*
       3400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE  DADREL
                  RELATO.

           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*****************BVVE0012****************'.
           DISPLAY '***                                   ***'.
           DISPLAY '***         PROCESSAMENTO  OK         ***'.
           DISPLAY '***                                   ***'.
           MOVE ACU-LID-DADREL     TO WRK-MASCARA.
           DISPLAY '*** LIDOS  DADAREL   : ' WRK-MASCARA ' ***'.
           DISPLAY '***                                   ***'.
           DISPLAY '***                                   ***'.
           DISPLAY '*****************BVVE0012****************'.


      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE0012'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

