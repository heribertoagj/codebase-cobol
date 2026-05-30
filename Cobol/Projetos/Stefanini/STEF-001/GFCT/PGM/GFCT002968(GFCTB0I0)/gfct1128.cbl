      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID.      GFCT1128.
       AUTHOR.          IARA C. BIZELLA PALOMARO.
      *================================================================*
      *      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1128                                    *
      *    PROGRAMADOR.:   IARA C. BIZELLA PALOMARO    - SONDPROC/GP.50*
      *    ANALISTA....:   IARA C. BIZELLA PALOMARO    - SONDPROC/GP.50*
      *    DATA........:   14/11/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   RELATORIO ANALITICO MENSAL DE VALORES DE    *
      *                    TARIFAS FLEXIBILIZADAS                      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                DEPARTOS                    GFCTWADI            *
      *                RELATO                      -                   *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      TABLE DB2                             INCLUDE/BOOK        *
      *                 DB2PRD.V01CLIENTE_AGENCIA    CLIEV004          *
      *                 DB2PRD.V01CTA_POUPANCA       CLIEV007          *
      *                 DB2PRD.V01CTA_CORRENTE       CLIEV008          *
      *                                                                *
      *   MODULOS CHAMADOS:                                            *
      *   BRAD0431 - CALCULO DO DIGITO DA AGENCIA / CONTA.             *
      *   BRAD7100 - TRATAMENTO DE ERRO DB2.                           *
      *   BRAD7600 - OBTEM DATA E HORA DO SISTEMA.                     *
      *   MESU9300 - OBTER DESCRICAO DE DEPARTAMENTO E AGENCIA.        *
      *================================================================*
           EJECT
      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT  DEPARTOS   ASSIGN  TO  UT-S-DEPARTOS
           FILE STATUS                IS  WRK-FS-DEPARTOS.

           SELECT  RELATO     ASSIGN  TO  UT-S-RELATO
           FILE STATUS                IS  WRK-FS-RELATO.

      *================================================================*
       DATA DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  INPUT....: LAYOUT DO ARQUIVO DEPARTOS                         *
      *             ORG. SEQUENCIAL    -  LRECL = 157 BYTES            *
      *----------------------------------------------------------------*

       FD  DEPARTOS
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

       COPY 'GFCTWADI'.

      *----------------------------------------------------------------*
      *  OUTPUT...: LAYOUT DO RELATO -                                 *
      *             ORG. SEQUENCIAL    -  LRECL = 133 BYTES            *
      *----------------------------------------------------------------*

       FD  RELATO
           LABEL       RECORD    STANDARD
           RECORDING   MODE      F
           BLOCK       CONTAINS  0.

       01  FD-REG-RELATO               PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           'INICIO DA WORKING STORAGE GFCT1128'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREAS ACUMULADORES'.

       01  ACU-LIDOS-DEPARTOS      PIC  9(009)    COMP-3 VALUE ZEROS.
       01  ACU-GRAVADOS-RELATO     PIC  9(009)    COMP-3 VALUE ZEROS.
       01  ACU-PAGINAS             PIC  9(003)    COMP-3 VALUE ZEROS.
       01  ACU-LINHAS              PIC  9(002)    COMP-3 VALUE 90.

      *----------------------------------------------------------------*
       01  FILLER                     PIC  X(050) VALUE
           'AREAS PARA TESTE DE FILE-STATUS'.

       01  WRK-FS-DEPARTOS            PIC  X(002) VALUE SPACES.
       01  WRK-FS-RELATO              PIC  X(002) VALUE SPACES.
       01  WRK-ABERTURA               PIC  X(013) VALUE 'NA ABERTURA'.
       01  WRK-LEITURA                PIC  X(013) VALUE 'NA LEITURA'.
       01  WRK-GRAVACAO               PIC  X(013) VALUE 'NA GRAVACAO'.
       01  WRK-FECHAMENTO             PIC  X(013) VALUE 'NO FECHAMENTO'.
       01  WRK-BATCH                  PIC  X(008) VALUE 'BATCH'.

       01  WRK-MENSAGEM-ERRO.
           05  FILLER                 PIC  X(009) VALUE '*** ERRO '.
           05  WRK-OPERACAO           PIC  X(013) VALUE SPACES.
           05  FILLER                 PIC  X(012) VALUE ' DO ARQUIVO '.
           05  WRK-NOME-ARQUIVO       PIC  X(008) VALUE SPACES.
           05  FILLER                 PIC  X(017) VALUE
               ' - FILE STATUS = '.
           05  WRK-FS                 PIC  X(002) VALUE SPACES.
           05  FILLER                 PIC  X(004) VALUE ' ***'.
           05  FILLER                 PIC  X(010) VALUE SPACES.

       COPY 'I#BRAD7C'.

       01  WRK-TIPO-ACESSO            PIC  X(003) VALUE SPACES.

       01  WRK-ERR-TEXTO              PIC  X(075) VALUE SPACES.
       01  WRK-NOME-MESU9300          PIC  X(050) VALUE SPACES.
       01  WRK-MODULO                 PIC  X(008) VALUE SPACES.
       01  WRK-0431-ENTRADA           PIC  9(013) COMP-3  VALUE ZEROS.
       01  WRK-0431-SAIDA             PIC  X(001) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREAS PARA OBTER DATA E HORA DO SISTEMA'.

       01  WRK-DATA-HORA.
           05  WRK-DT-JULIANA          PIC  9(005) COMP-3 VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC  9(007) COMP-3 VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC  9(007) COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3 VALUE ZEROS.
           05  WRK-TIMESTAMP           PIC  X(020)        VALUE SPACES.

      *----------------------------------------------------------------*
      *    CAMPOS AUXILIARES DE DATA                                   *
      *----------------------------------------------------------------*

       01  WRK-DDMMAAAA.
           05  WRK-DIA-EDT             PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '/'.
           05  WRK-MES-EDT             PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '/'.
           05  WRK-ANO-EDT             PIC  9(004) VALUE ZEROS.
       01  WRK-DDMMAAAA-R REDEFINES    WRK-DDMMAAAA
                                       PIC  X(010).

       01  WRK-AAAAMMDD                PIC  9(009) VALUE ZEROS.
       01  WRK-AAAAMMDD-R REDEFINES    WRK-AAAAMMDD.
           05  FILLER                  PIC  9(001).
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-HORA-EDITADA.
           05  WRK-HH-EDT              PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE ':'.
           05  WRK-MM-EDT              PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE ':'.
           05  WRK-SS-EDT              PIC  9(002) VALUE ZEROS.
       01  WRK-HORA-EDITADA-R          REDEFINES  WRK-HORA-EDITADA
                                       PIC  X(008).

       01  WRK-HORA                    PIC  9(007) VALUE ZEROS.
       01  WRK-HORA-R    REDEFINES     WRK-HORA.
           05  FILLER                  PIC  9(001).
           05  WRK-HH                  PIC  9(002).
           05  WRK-MM                  PIC  9(002).
           05  WRK-SS                  PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'AREAS AUXILIARES'.

       01  WRK-MASCARA                 PIC  ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       01  WRK-DEPART-ATU              PIC  9(005) COMP-3 VALUE ZEROS.
       01  WRK-DEPART-ANT              PIC  9(005) COMP-3 VALUE ZEROS.
       01  WRK-JUNCAO-ATU              PIC  9(005) COMP-3 VALUE ZEROS.
       01  WRK-JUNCAO-ANT              PIC  9(005) COMP-3 VALUE ZEROS.

       01  WRK-DATA-REFERENCIA.
           03  WRK-DIA-REF             PIC  X(002)       VALUE SPACES.
           03  FILLER                  PIC  X(001)       VALUE SPACES.
           03  WRK-MES-REF             PIC  X(002)       VALUE SPACES.
           03  FILLER                  PIC  X(001)       VALUE SPACES.
           03  WRK-ANO-REF             PIC  X(004)       VALUE SPACES.

       01  WRK-DT-REF.
           03  WRK-MM-REF              PIC  X(002)       VALUE SPACES.
           03  FILLER                  PIC  X(001)       VALUE '/'.
           03  WRK-AA-REF              PIC  X(004)       VALUE SPACES.

       01  WRK-JUNCAO-R.
           03  WRK-NUM-JUNCAO          PIC  9(005)        VALUE ZEROS.
           03  FILLER                  PIC  X(001)        VALUE '-'.
           03  WRK-DIG-JUNCAO          PIC  X(001)        VALUE SPACES.
       01  WRK-JUNCAO       REDEFINES  WRK-JUNCAO-R
                                       PIC  X(007).

       01  WRK-NR-CONTA-R.
           03  WRK-NUM-CONTA           PIC  9(013)        VALUE ZEROS.
           03  FILLER                  PIC  X(001)        VALUE '-'.
           03  WRK-DIG-CONTA           PIC  X(001)        VALUE SPACES.
       01  WRK-NR-CONTA     REDEFINES  WRK-NR-CONTA-R
                                       PIC  X(015).

       01  WRK-NUM-CONTA-ANT           PIC  9(013)        VALUE ZEROS.

       01  WRK-CPF.
           03  WRK-CPF-NUM             PIC  X(09)         VALUE SPACES.
           03  FILLER                  PIC  X(001)        VALUE '-'.
           03  WRK-CPF-DIG             PIC  9(002)        VALUE ZEROS.
           03  FILLER                  PIC  X(005)        VALUE SPACES.

       01  WRK-CNPJ.
ST25X6     03  WRK-CNPJ-NUM            PIC  X(09)         VALUE SPACES.
           03  FILLER                  PIC  X(001)        VALUE '/'.
ST25X6     03  WRK-CNPJ-FIL            PIC  X(004)        VALUE SPACES.
           03  FILLER                  PIC  X(001)        VALUE '-'.
           03  WRK-CNPJ-DIG            PIC  9(002)        VALUE ZEROS.

       01  WRK-CNPJ-CPF                PIC  X(019)        VALUE SPACES.
       01  WRK-NOME-CLIENTE            PIC  X(030)        VALUE SPACES.
       01  WRK-DATA-DEBITO             PIC  X(010)        VALUE SPACES.
       01  WRK-VALOR-DESCONTO          PIC  9(015)V9(002) VALUE ZEROS.
       01  WRK-TOT-AGENCIA-REL         PIC  9(015)V9(002) VALUE ZEROS.
       01  WRK-TOT-GERAL-REL           PIC  9(015)V9(002) VALUE ZEROS.

       01  WRK-TOTAL-AGENCIA     PIC  9(015)V9(002) COMP-3 VALUE ZEROS.
       01  WRK-TOTAL-GERAL       PIC  9(015)V9(002) COMP-3 VALUE ZEROS.

       01  WRK-QUEBRA-DEPART           PIC  X(001)        VALUE 'N'.

       01  WRK-NOME-CLIEV004           PIC  X(070)        VALUE SPACES.

       01  WRK-NOME-CLIEV004-R  REDEFINES  WRK-NOME-CLIEV004.
           05  WRK-NOME-PARTE1         PIC  X(030).
           05  FILLER                  PIC  X(040).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(40)           VALUE
           '*       AREA DO MODULO MESU9300       *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-MESU9300.
           05  WRK-9300-ENTRADA.
               10  WRK-9300-NOME-PGM   PIC X(08)           VALUE SPACES.
               10  WRK-9300-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               10  WRK-9300-BANCO      PIC 9(03)           VALUE ZEROS.
               10  WRK-9300-AGENCIA    PIC 9(05)           VALUE ZEROS.
           05  WRK-9300-RETORNO.
               10  WRK-9300-DAC-AGENCIA
                                       PIC X(01)           VALUE SPACES.
               10  WRK-9300-NOME-AGENC PIC X(40)           VALUE SPACES.
               10  WRK-9300-ENDER-AGENC
                                       PIC X(30)           VALUE SPACES.
               10  WRK-9300-DIRETORIA-REG
                                       PIC 9(05)           VALUE ZEROS.
               10  WRK-9300-DIRETORIA-NOME
                                       PIC X(40)           VALUE SPACES.
               10  WRK-9300-NOME-BANCO PIC X(40)           VALUE SPACES.
               10  WRK-9300-POLO-SERVICO
                                       PIC 9(05)           VALUE ZEROS.
               10  WRK-9300-MUNIC-AGENC
                                       PIC X(25)           VALUE SPACES.
               10  WRK-9300-UF         PIC X(02)           VALUE SPACES.
               10  WRK-9300-CEP        PIC 9(05)           VALUE ZEROS.
               10  WRK-9300-CEP-COMPL  PIC 9(03)           VALUE ZEROS.
               10  WRK-9300-INDIC-AGENC-AT
                                       PIC X(01)           VALUE SPACES.
               10  WRK-9300-NATUREZA-AGENC
                                       PIC X(01)           VALUE SPACES.
               10  WRK-9300-FILLER     PIC X(50)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*        AREA PARA BRAD0431          *'.
      *----------------------------------------------------------------*

       01  WRK-0431-CONTA              PIC  9(013)         VALUE ZEROS.
       01  WRK-0431-DIGITO-CONTA       PIC  X(001)         VALUE SPACES.
       01  WRK-0431-TAMANHO            PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           'LAYOUT DO RELATORIO RELATO'.

      *----------------------------------------------------------------*
      *    LINHAS DE CABECALHO DO RELATO                               *
      *----------------------------------------------------------------*

       01  CABEC1-REL.
           05  CB1-REL-CARRO           PIC  X(001) VALUE '1'.
           05  CB1-REL-DATA            PIC  X(010) VALUE SPACES.
           05  FILLER                  PIC  X(039) VALUE SPACES.
           05  FILLER                  PIC  X(031) VALUE
               'B A N C O  B R A D E S C O  S/A'.
           05  FILLER                  PIC  X(044) VALUE SPACES.
           05  CB1-REL-HORA            PIC  X(008) VALUE SPACES.

       01  CABEC2-REL.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(008) VALUE 'GFCT1128'.
           05  FILLER                  PIC  X(031) VALUE SPACES.
           05  FILLER                  PIC  X(051) VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           05  FILLER                  PIC  X(034) VALUE SPACES.
           05  FILLER                  PIC  X(005) VALUE 'PAG. '.
           05  CB2-REL-NPAG            PIC  9(003) VALUE ZEROS.

       01  CABEC3-REL.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(042) VALUE SPACES.
           05  FILLER                  PIC  X(046) VALUE
               'RELATORIO ANALITICO DAS TARIFAS FLEXIBILIZADAS'.
           05  FILLER                  PIC  X(044) VALUE SPACES.

       01  CABEC4-REL.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(058) VALUE SPACES.
           05  FILLER                  PIC  X(006) VALUE
               'MES : '.
           05  CB4-DT-REFERENC         PIC  X(007) VALUE SPACES.
           05  FILLER                  PIC  X(061) VALUE SPACES.

       01  CABEC-TRACOS.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(132) VALUE ALL '-'.

       01  CABEC-BRANCO.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(132) VALUE SPACES.

      *----------------------------------------------------------------*
      *    LINHAS DE QUEBRAS DO RELATO                                 *
      *----------------------------------------------------------------*

       01  CABEC-QUEBRA.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(014) VALUE
           'DEPARTAMENTO: '.
           05  CB-DEPTO                PIC  9(005) VALUE ZEROS.
           05  FILLER                  PIC  X(003) VALUE ' - ' .
           05  CB-DEPTO-NOME           PIC  X(050) VALUE SPACES.
           05  FILLER                  PIC  X(060) VALUE SPACES.

      *----------------------------------------------------------------*
      *    LINHAS DE TITULOS DA LISTA DO RELATO                        *
      *----------------------------------------------------------------*

       01  CABEC-TITULO-1.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(007) VALUE
           ' JUNCAO'.
           05  FILLER                  PIC  X(006) VALUE SPACES.
           05  FILLER                  PIC  X(012) VALUE
           'NOME AGENCIA'.
           05  FILLER                  PIC  X(107) VALUE SPACES.

       01  CABEC-TITULO-2.
           05  FILLER                  PIC  X(001) VALUE SPACES.
           05  FILLER                  PIC  X(009) VALUE
           ' CONTA NR'.
           05  FILLER                  PIC  X(008) VALUE SPACES.
           05  FILLER                  PIC  X(008) VALUE
           'CPF/CNPJ'.
           05  FILLER                  PIC  X(013) VALUE SPACES.
           05  FILLER                  PIC  X(015) VALUE
           'NOME DO CLIENTE'.
           05  FILLER                  PIC  X(017) VALUE SPACES.
           05  FILLER                  PIC  X(014) VALUE
           'DATA DE DEBITO'.
           05  FILLER                  PIC  X(004) VALUE SPACES.
           05  FILLER                  PIC  X(017) VALUE
           'VALOR DO DESCONTO'.
           05  FILLER                  PIC  X(017) VALUE SPACES.
           05  FILLER                  PIC  X(005) VALUE
           'TOTAL'.
           05  FILLER                  PIC  X(005) VALUE SPACES.

      *----------------------------------------------------------------*
      *    LINHAS DE DETALHE DO RELATO                                 *
      *----------------------------------------------------------------*

       01  LINDET1-REL.
           05  FILLER                  PIC  X(001)    VALUE SPACES.
           05  FILLER                  PIC  X(001)    VALUE SPACES.
           05  LD-JUNCAO               PIC  X(007)    VALUE SPACES.
           05  FILLER                  PIC  X(005)    VALUE SPACES.
           05  LD-NOME-AGENCIA         PIC  X(050)    VALUE SPACES.
           05  FILLER                  PIC  X(069)    VALUE SPACES.

       01  LINDET2-REL.
           05  FILLER                  PIC  X(001)    VALUE SPACES.
           05  FILLER                  PIC  X(001)    VALUE SPACES.
           05  LD-NR-CONTA             PIC  X(015)    VALUE SPACES.
           05  FILLER                  PIC  X(001)    VALUE SPACES.
           05  LD-CPF-CNPJ             PIC  X(019)    VALUE SPACES.
           05  FILLER                  PIC  X(002)    VALUE SPACES.
           05  LD-NOME-CLIENTE         PIC  X(030)    VALUE SPACES.
           05  FILLER                  PIC  X(004)    VALUE SPACES.
           05  LD-DATA-DEBITO          PIC  X(010)    VALUE SPACES.
           05  FILLER                  PIC  X(003)    VALUE SPACES.
           05  LD-VALOR-DESCONTO       PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                      VALUE ZEROS.
           05  FILLER                  PIC  X(003)    VALUE SPACES.
           05  LD-TOTAL-AGENCIA        PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZZ,ZZ
                                                      VALUE ZEROS.

      *----------------------------------------------------------------*
      *    LINHAS DE TOTAL GERAL DO RELATO                             *
      *----------------------------------------------------------------*

       01  LINTOT1-REL.
           05  FILLER                  PIC  X(001)    VALUE SPACES.
           05  FILLER                  PIC  X(076)    VALUE SPACES.
           05  FILLER                  PIC  X(034)    VALUE
           'VALOR TOTAL DE DESCONTOS NO MES : '.
           05  LD-TOTAL-GERAL          PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99
                                                      VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*              AREA PARA TABELA DB2              *'.
      *----------------------------------------------------------------*
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV008
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50) VALUE
           'FIM DA WORKING GFCT1128'.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *  INICIO DO PROCESSAMENTO                                       *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM  1000-INICIALIZAR.

           PERFORM  2000-VERIFICAR-VAZIO.

           PERFORM  1200-OBTER-DATA-HORA.

           PERFORM  3000-PROCESSAR         UNTIL
                         WRK-FS-DEPARTOS   EQUAL   '10'.

           PERFORM  5000-TOTALIZAR.

           PERFORM  6000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ABRIR ARQUIVOS E TESTAR FILE-STATUS                           *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN  INPUT  DEPARTOS
                 OUTPUT RELATO.

           MOVE  WRK-ABERTURA          TO  WRK-OPERACAO.

           PERFORM  1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DOS ARQUIVOS                               *
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM  1110-TESTAR-FS-DEPARTOS.

           PERFORM  1130-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO DEPARTOS                        *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-DEPARTOS          SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-DEPARTOS   NOT     EQUAL   '00'   AND '04'
              DISPLAY '************** GFCT1128 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              DEPARTOS             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-DEPARTOS
                                                 '         *'
              DISPLAY '************** GFCT1128 *************'
              MOVE  'APL'               TO  WRK-TIPO-ACESSO
              MOVE  'DEPARTOS'          TO  WRK-NOME-ARQUIVO
              MOVE  WRK-FS-DEPARTOS     TO  WRK-FS
              MOVE  WRK-MENSAGEM-ERRO   TO  WRK-ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  TESTAR FILE-STATUS DO ARQUIVO RELATO                          *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-RELATO    NOT     EQUAL   '00'
              DISPLAY '************** GFCT1128 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              RELATO               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-RELATO
                                                 '         *'
              DISPLAY '************** GFCT1128 *************'
              MOVE  'APL'              TO  WRK-TIPO-ACESSO
              MOVE  'RELATO'           TO  WRK-NOME-ARQUIVO
              MOVE  WRK-FS-RELATO      TO  WRK-FS
              MOVE  WRK-MENSAGEM-ERRO  TO  WRK-ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  OBTEM DATA E HORA DO SISTEMA                                  *
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD7600'            USING   WRK-DATA-HORA.

           MOVE  WRK-DT-AAAAMMDD       TO  WRK-AAAAMMDD.
           MOVE  WRK-ANO-INV           TO  WRK-ANO-EDT.
           MOVE  WRK-MES-INV           TO  WRK-MES-EDT.
           MOVE  WRK-DIA-INV           TO  WRK-DIA-EDT.
           MOVE  WRK-DDMMAAAA-R        TO  CB1-REL-DATA.

           MOVE  WRK-TI-HHMMSS         TO  WRK-HORA.
           MOVE  WRK-HH                TO  WRK-HH-EDT.
           MOVE  WRK-MM                TO  WRK-MM-EDT.
           MOVE  WRK-SS                TO  WRK-SS-EDT.
           MOVE  WRK-HORA-EDITADA-R    TO  CB1-REL-HORA.

      *----------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  VERIFICAR ARQUIVOS VAZIOS                                     *
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM  2100-VERIFICAR-VAZIO-DEPARTOS.

      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-VERIFICAR-VAZIO-DEPARTOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM  2150-LER-DEPARTOS.

           IF  ACU-LIDOS-DEPARTOS      EQUAL   ZEROS
               DISPLAY  '********* GFCT1128 *********'
               DISPLAY  '*     ARQUIVO  DEPARTOS    *'
               DISPLAY  '*          VAZIO,          *'
               DISPLAY  '* PROCESSAMENTO ENCERRADO  *'
               DISPLAY  '********* GFCT1128 *********'
               CLOSE  DEPARTOS
                      RELATO
               MOVE  WRK-FECHAMENTO TO WRK-OPERACAO
               PERFORM  1100-TESTAR-FILE-STATUS
               STOP RUN
           END-IF.

           MOVE GFCTWADI-CAG-DSTNO      TO WRK-NUM-JUNCAO.
           MOVE GFCTWADI-CCTA-DSTNO     TO WRK-NUM-CONTA.
           MOVE GFCTWADI-DEFETV-DEB     TO WRK-DATA-DEBITO.
           MOVE GFCTWADI-VCONCS-DESC    TO WRK-VALOR-DESCONTO.
           ADD  GFCTWADI-VCONCS-DESC    TO WRK-TOTAL-AGENCIA.
           ADD  GFCTWADI-VCONCS-DESC    TO WRK-TOTAL-GERAL.

           MOVE  GFCTWADI-DEFETV-DEB    TO WRK-DATA-REFERENCIA.
           MOVE  WRK-MES-REF            TO WRK-MM-REF.
           MOVE  WRK-ANO-REF            TO WRK-AA-REF.

      *----------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA LER DEPARTOS                                        *
      *----------------------------------------------------------------*
       2150-LER-DEPARTOS                SECTION.
      *----------------------------------------------------------------*

           READ  DEPARTOS.

           IF  WRK-FS-DEPARTOS          EQUAL '10'
               GO                       TO 2150-99-FIM
           END-IF.

           MOVE  WRK-LEITURA            TO WRK-OPERACAO.
           PERFORM  1110-TESTAR-FS-DEPARTOS.

           MOVE GFCTWADI-CDEPDC         TO WRK-DEPART-ATU
                                           CB-DEPTO.

           MOVE GFCTWADI-CAG-DSTNO      TO WRK-JUNCAO-ATU.

           ADD   1                      TO ACU-LIDOS-DEPARTOS.

      *----------------------------------------------------------------*
       2150-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA PROCESSAR                                           *
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF  WRK-DEPART-ATU  NOT EQUAL WRK-DEPART-ANT
               IF  WRK-DEPART-ANT  NOT EQUAL  ZEROS
                   PERFORM  4300-QUEBRAR-DEPARTAMENTO
               END-IF
               PERFORM  3200-IMPRIMIR-QUEBRA
               PERFORM  3400-IMPRIMIR-DETALHE-1
               MOVE  WRK-DEPART-ATU  TO  WRK-DEPART-ANT
               MOVE  WRK-JUNCAO-ATU  TO  WRK-JUNCAO-ANT
           ELSE
               IF  WRK-JUNCAO-ATU  NOT EQUAL  WRK-JUNCAO-ANT
                   IF  WRK-JUNCAO-ANT  NOT EQUAL  ZEROS
                       PERFORM  3300-IMPRIMIR-RELATO
                       PERFORM  4400-QUEBRAR-JUNCAO
                   END-IF
                   PERFORM  3400-IMPRIMIR-DETALHE-1
                   MOVE  WRK-JUNCAO-ATU    TO  WRK-JUNCAO-ANT
               ELSE
                   PERFORM  3300-IMPRIMIR-RELATO
                   MOVE   ZEROS            TO  LD-TOTAL-AGENCIA
                   PERFORM  3500-IMPRIMIR-DETALHE-2
                   PERFORM  4200-TRATAR-REGISTRO-LIDO
               END-IF
           END-IF.

           PERFORM  2150-LER-DEPARTOS.

      *----------------------------------------------------------------*
       3000-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA IMPRIMIR CABEC                                      *
      *----------------------------------------------------------------*
       3100-IMPRIMIR-CABEC             SECTION.
      *----------------------------------------------------------------*

           ADD   1                     TO   ACU-PAGINAS.
           MOVE  ACU-PAGINAS           TO   CB2-REL-NPAG.

           WRITE FD-REG-RELATO         FROM CABEC1-REL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC2-REL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC3-REL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           MOVE  WRK-DT-REF            TO  CB4-DT-REFERENC.
           WRITE FD-REG-RELATO         FROM CABEC4-REL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-TRACOS.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           MOVE  7                     TO   ACU-LINHAS.

      *----------------------------------------------------------------*
       3100-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-IMPRIMIR-QUEBRA             SECTION.
      *----------------------------------------------------------------*

           PERFORM  3100-IMPRIMIR-CABEC.
           PERFORM  3600-BUSCA-NOME-DEPTO.

           WRITE FD-REG-RELATO         FROM CABEC-QUEBRA.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-TRACOS.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-TITULO-1.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-TITULO-2.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           ADD  08                     TO   ACU-LINHAS.

      *----------------------------------------------------------------*
       3200-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA IMPRIMIR RELATORIO                                  *
      *----------------------------------------------------------------*
       3300-IMPRIMIR-RELATO            SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS          GREATER 55
RIC            PERFORM  3200-IMPRIMIR-QUEBRA
           END-IF.

      *----------------------------------------------------------------*
       3300-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-IMPRIMIR-DETALHE-1         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-NUM-JUNCAO           TO  WRK-0431-ENTRADA.
           PERFORM  3800-OBTER-DIGITO.
           MOVE  WRK-0431-SAIDA           TO  WRK-DIG-JUNCAO.
           MOVE  WRK-JUNCAO               TO LD-JUNCAO.

           PERFORM  3700-BUSCA-NOME-AGENCIA.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM LINDET1-REL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           ADD   3                        TO ACU-LINHAS.

           MOVE  'S'                      TO  WRK-QUEBRA-DEPART.

      *----------------------------------------------------------------*
       3400-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3500-IMPRIMIR-DETALHE-2         SECTION.
      *----------------------------------------------------------------*

           IF  (WRK-NUM-CONTA          EQUAL  WRK-NUM-CONTA-ANT)   AND
               (WRK-QUEBRA-DEPART  NOT EQUAL  'S')
               MOVE  SPACES                TO  LD-NR-CONTA
               MOVE  SPACES                TO  LD-CPF-CNPJ
               MOVE  SPACES                TO  LD-NOME-CLIENTE
           ELSE
               MOVE  WRK-NUM-CONTA         TO WRK-0431-ENTRADA
               PERFORM  3800-OBTER-DIGITO
               MOVE  WRK-0431-SAIDA        TO WRK-DIG-CONTA
               MOVE  WRK-NR-CONTA          TO LD-NR-CONTA
               PERFORM  3900-OBTER-CPF-CNPJ-NOME
               MOVE  WRK-CNPJ-CPF          TO LD-CPF-CNPJ
               MOVE  WRK-NOME-CLIENTE      TO LD-NOME-CLIENTE
               MOVE  WRK-NUM-CONTA         TO WRK-NUM-CONTA-ANT
               MOVE  'N'                   TO WRK-QUEBRA-DEPART
           END-IF.

           MOVE  WRK-DATA-DEBITO          TO LD-DATA-DEBITO.
           MOVE  WRK-VALOR-DESCONTO       TO LD-VALOR-DESCONTO.

           WRITE FD-REG-RELATO         FROM LINDET2-REL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           ADD   1                        TO ACU-LINHAS
                                             ACU-GRAVADOS-RELATO.

      *----------------------------------------------------------------*
       3500-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ACESSO AO MODULO MESU9300                                  *
      *----------------------------------------------------------------*
       3600-BUSCA-NOME-DEPTO           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-AREA-MESU9300.

           MOVE WRK-DEPART-ATU         TO WRK-9300-AGENCIA.

           MOVE 'GFCT1128'             TO WRK-9300-NOME-PGM.
           MOVE 237                    TO WRK-9300-BANCO.
           MOVE ZEROS                  TO WRK-9300-COD-RETORNO.
           MOVE 'MESU9300'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-AREA-MESU9300
                                             ERRO-AREA.

           IF  WRK-9300-COD-RETORNO    EQUAL ZEROS
               MOVE WRK-9300-NOME-AGENC
                                       TO WRK-NOME-MESU9300
           ELSE
               IF  WRK-9300-COD-RETORNO
                                       EQUAL 1
                   MOVE 'NAO ENCONTRADA'
                                       TO WRK-NOME-MESU9300
               ELSE
                   IF  WRK-9300-COD-RETORNO
                                       EQUAL 2
                       MOVE 'APL'      TO WRK-TIPO-ACESSO
                       STRING '             PARAMETROS INVALIDOS '
                              'NO ACESSO AO MODULO MESU9300'
                       DELIMITED BY SIZE INTO WRK-ERR-TEXTO
                   END-IF
                   PERFORM 9999-ROTINA-ERRO
               END-IF
           END-IF.

           MOVE WRK-NOME-MESU9300      TO CB-DEPTO-NOME.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3700-BUSCA-NOME-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-AREA-MESU9300.

           MOVE WRK-JUNCAO-ATU         TO WRK-9300-AGENCIA.

           MOVE 'GFCT1128'             TO WRK-9300-NOME-PGM.
           MOVE 237                    TO WRK-9300-BANCO.
           MOVE ZEROS                  TO WRK-9300-COD-RETORNO.
           MOVE 'MESU9300'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-AREA-MESU9300
                                             ERRO-AREA.

           IF  WRK-9300-COD-RETORNO    EQUAL ZEROS
               MOVE WRK-9300-NOME-AGENC
                                       TO WRK-NOME-MESU9300
           ELSE
               IF  WRK-9300-COD-RETORNO
                                       EQUAL 1
                   MOVE 'NAO ENCONTRADA'
                                       TO WRK-NOME-MESU9300
               ELSE
                   IF  WRK-9300-COD-RETORNO
                                       EQUAL 2
                       MOVE 'APL'      TO WRK-TIPO-ACESSO
                       STRING '             PARAMETROS INVALIDOS '
                              'NO ACESSO AO MODULO MESU9300'
                       DELIMITED BY SIZE INTO WRK-ERR-TEXTO
                   END-IF
                   PERFORM 9999-ROTINA-ERRO
               END-IF
           END-IF.

           MOVE WRK-NOME-MESU9300      TO LD-NOME-AGENCIA.

      *----------------------------------------------------------------*
       3700-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER DIGITO DO CONTA DO CLIENTE E DA AGENCIA                  *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3800-OBTER-DIGITO               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-0431-ENTRADA       TO WRK-0431-CONTA.
           MOVE SPACES                 TO WRK-0431-DIGITO-CONTA.
           MOVE 13                     TO WRK-0431-TAMANHO.

           CALL  'BRAD0431'            USING WRK-0431-CONTA
                                             WRK-0431-DIGITO-CONTA
                                             WRK-0431-TAMANHO.

           IF  WRK-0431-DIGITO-CONTA   EQUAL  '.'
               MOVE 'APL'      TO WRK-TIPO-ACESSO
               STRING '             PARAMETROS INVALIDOS '
                      'NO ACESSO AO MODULO BRAD0431'
               DELIMITED BY SIZE INTO WRK-ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-0431-DIGITO-CONTA  TO WRK-0431-SAIDA.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * OBTER CPF/CNPJ E NOME DO CLIENTE                               *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3900-OBTER-CPF-CNPJ-NOME        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-NUM-CONTA  GREATER  999999
               PERFORM  4000-ACESSAR-CLIEV007-CLIEV004
           ELSE
               PERFORM  4100-ACESSAR-CLIEV008-CLIEV004
           END-IF.

           IF  CFLIAL-CGC OF CLIEV004  EQUAL  ZEROS
               MOVE  CCGC-CPF       OF  CLIEV004  TO  WRK-CPF-NUM
               MOVE  CCTRL-CPF-CGC  OF  CLIEV004  TO  WRK-CPF-DIG
               MOVE  WRK-CPF        TO  WRK-CNPJ-CPF
           ELSE
               MOVE  CCGC-CPF       OF  CLIEV004  TO  WRK-CNPJ-NUM
               MOVE  CFLIAL-CGC    OF  CLIEV004  TO  WRK-CNPJ-FIL
               MOVE  CCTRL-CPF-CGC  OF  CLIEV004  TO  WRK-CNPJ-DIG
               MOVE  WRK-CNPJ       TO  WRK-CNPJ-CPF
           END-IF.

           MOVE  IPSSOA-COPLT   OF  CLIEV004  TO  WRK-NOME-CLIEV004.

           MOVE  WRK-NOME-PARTE1              TO  WRK-NOME-CLIENTE.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-ACESSAR-CLIEV007-CLIEV004  SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-NUM-JUNCAO   TO  CJUNC-DEPDC  OF  CLIEV007.
           MOVE  WRK-NUM-CONTA    TO  CCTA-CLI     OF  CLIEV007.

           EXEC SQL SELECT
                   CID_CLI
           INTO
                   :CLIEV007.CID-CLI
           FROM
                   DB2PRD.V01CTA_POUPANCA
           WHERE
                   CJUNC_DEPDC = :CLIEV007.CJUNC-DEPDC  AND
                   CCTA_CLI    = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE        NOT EQUAL  ZEROS)  OR
               (SQLWARN0           EQUAL  'W')
               MOVE  'DB2'                TO  ERR-TIPO-ACESSO
                                              WRK-TIPO-ACESSO
               MOVE  'V01CTA_POUPANCA'    TO  ERR-DBD-TAB
               MOVE  'SELECT'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE              TO  ERR-SQL-CODE
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           EXEC SQL SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
                   IPSSOA_COPLT
           INTO
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
                   :CLIEV004.IPSSOA-COPLT
           FROM
                   DB2PRD.V01CLIENTE_AGENCIA
           WHERE
                   CID_CLI    = :CLIEV007.CID-CLI
           END-EXEC.

           IF  (SQLCODE        NOT EQUAL  ZEROS)  OR
               (SQLWARN0           EQUAL  'W')
               MOVE  'DB2'                TO  ERR-TIPO-ACESSO
                                              WRK-TIPO-ACESSO
               MOVE  'V01CLIENTE_AGENCIA' TO  ERR-DBD-TAB
               MOVE  'SELECT'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE              TO  ERR-SQL-CODE
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-ACESSAR-CLIEV008-CLIEV004  SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-NUM-JUNCAO   TO  CJUNC-DEPDC  OF  CLIEV008.
           MOVE  WRK-NUM-CONTA    TO  CCTA-CLI     OF  CLIEV008.

           EXEC SQL SELECT
                   CID_CLI
           INTO
                   :CLIEV008.CID-CLI
           FROM
                   DB2PRD.V01CTA_CORRENTE
           WHERE
                   CJUNC_DEPDC = :CLIEV008.CJUNC-DEPDC  AND
                   CCTA_CLI    = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE        NOT EQUAL  ZEROS)  OR
               (SQLWARN0           EQUAL  'W')
               MOVE  'DB2'                TO  ERR-TIPO-ACESSO
                                              WRK-TIPO-ACESSO
               MOVE  'V01CTA_CORRENTE'    TO  ERR-DBD-TAB
               MOVE  'SELECT'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE              TO  ERR-SQL-CODE
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           EXEC SQL SELECT
                   CCGC_CPF,
                   CFLIAL_CGC,
                   CCTRL_CPF_CGC,
                   IPSSOA_COPLT
           INTO
                   :CLIEV004.CCGC-CPF,
                   :CLIEV004.CFLIAL-CGC,
                   :CLIEV004.CCTRL-CPF-CGC,
                   :CLIEV004.IPSSOA-COPLT
           FROM
                   DB2PRD.V01CLIENTE_AGENCIA
           WHERE
                   CID_CLI    = :CLIEV008.CID-CLI
           END-EXEC.

           IF  (SQLCODE        NOT EQUAL  ZEROS)  OR
               (SQLWARN0           EQUAL  'W')
               MOVE  'DB2'                TO  ERR-TIPO-ACESSO
                                              WRK-TIPO-ACESSO
               MOVE  'V01CLIENTE_AGENCIA' TO  ERR-DBD-TAB
               MOVE  'SELECT'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE              TO  ERR-SQL-CODE
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DO REGISTRO LIDO                        *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4200-TRATAR-REGISTRO-LIDO       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEPARTOS  NOT EQUAL  '10'
               MOVE GFCTWADI-CAG-DSTNO      TO WRK-NUM-JUNCAO
               MOVE GFCTWADI-CCTA-DSTNO     TO WRK-NUM-CONTA
               MOVE GFCTWADI-DEFETV-DEB     TO WRK-DATA-DEBITO
               MOVE GFCTWADI-VCONCS-DESC    TO WRK-VALOR-DESCONTO
               ADD  GFCTWADI-VCONCS-DESC    TO WRK-TOTAL-AGENCIA
               ADD  GFCTWADI-VCONCS-DESC    TO WRK-TOTAL-GERAL
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAR QUEBRA DE DEPARTAMENTO                              *
      *----------------------------------------------------------------*
       4300-QUEBRAR-DEPARTAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-TOTAL-AGENCIA    TO  WRK-TOT-AGENCIA-REL.
           MOVE  WRK-TOT-AGENCIA-REL  TO  LD-TOTAL-AGENCIA.

           PERFORM  3500-IMPRIMIR-DETALHE-2.

           MOVE  ZEROS                TO  WRK-TOTAL-AGENCIA
                                          WRK-TOT-AGENCIA-REL.

           PERFORM  5000-TOTALIZAR.

           PERFORM  4200-TRATAR-REGISTRO-LIDO.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAR QUEBRA DE JUNCAO                                    *
      *----------------------------------------------------------------*
       4400-QUEBRAR-JUNCAO         SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-TOTAL-AGENCIA    TO  WRK-TOT-AGENCIA-REL.
           MOVE  WRK-TOT-AGENCIA-REL  TO  LD-TOTAL-AGENCIA.

           PERFORM  3500-IMPRIMIR-DETALHE-2.

           MOVE  ZEROS                TO  WRK-TOTAL-AGENCIA
                                          WRK-TOT-AGENCIA-REL.

           IF  WRK-FS-DEPARTOS  EQUAL  '10'
               GO    TO                   4400-99-FIM
           END-IF.

           PERFORM  4200-TRATAR-REGISTRO-LIDO.

      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA TOTALIZAR RELATORIO                                 *
      *----------------------------------------------------------------*
       5000-TOTALIZAR              SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DEPARTOS  EQUAL  '10'
               PERFORM 3300-IMPRIMIR-RELATO
               PERFORM 4400-QUEBRAR-JUNCAO
           END-IF.

           MOVE  WRK-TOTAL-GERAL       TO  WRK-TOT-GERAL-REL.
           MOVE  WRK-TOT-GERAL-REL     TO  LD-TOTAL-GERAL.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM LINTOT1-REL.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CABEC-BRANCO.
           MOVE  WRK-GRAVACAO          TO   WRK-OPERACAO.
           PERFORM  1130-TESTAR-FS-RELATO.

           ADD   3                        TO ACU-LINHAS.

           MOVE  ZEROS                    TO WRK-TOTAL-GERAL.
           MOVE  ZEROS                    TO WRK-TOT-GERAL-REL.

      *----------------------------------------------------------------*
       5000-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     ROTINA FINALIZAR TOTAL                                     *
      *----------------------------------------------------------------*
       6000-FINALIZAR              SECTION.
      *----------------------------------------------------------------*

           PERFORM 6100-EMITIR-DISPLAY.

           CLOSE  DEPARTOS
                  RELATO.

           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO.
           PERFORM  1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       6000-99-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-DEPARTOS     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1128 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REGISTROS LIDOS EM DEPARTOS...: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-GRAVADOS-RELATO     TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO RELATO..: ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1128 ********************'.

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ROTINA DE ERRO                                                *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  ERR-DADOS-SENHAS.
           MOVE  'GFCT1128'            TO  ERR-PGM.
           MOVE  WRK-TIPO-ACESSO       TO  ERR-TIPO-ACESSO.

           IF  ERR-TIPO-ACESSO  NOT EQUAL  'DB2'
               MOVE  WRK-ERR-TEXTO     TO  ERR-TEXTO
           END-IF.

           CALL  'BRAD7100'            USING   WRK-BATCH
                                               ERRO-AREA
                                               SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*

