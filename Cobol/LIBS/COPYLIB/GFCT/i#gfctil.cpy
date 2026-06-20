      *****************************************************************
      * NOME DA INC - I#GFCTIL                                        *
      * DESCRICAO   - LAYOUT DO ARQUIVO DE REGISTROS ENVIADOS PARA    *
      *               COBRANCA NO MORA OU POUP                        *
      * TAMANHO     - 0300                                            *
      * DATA        - 02.2006                                         *
      * RESPONSAVEL - WAGNER - PROCWORK                               *
      *****************************************************************
      * MANUTENCAO - INCLUIR NUMERO DA PARCELA                        *
      * DATA        - 11.2010                                         *
      * RESPONSAVEL - PAGNOCCA - SONDA PROCWORK                       *
      *****************************************************************
BI0813* MANUTENCAO - INCLUIR CAMPO NOVO 'COBR-ULTIMO-REPIQUE'         *
BI0813* DATA        - 08.2013                                         *
BI0813* RESPONSAVEL - UBIRAJARA - SONDA PROCWORK                      *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 REG-COBRANCA.
          05 COBR-CHAVE-COBRANCA.
             10 COBR-DATA-ENVIO         PIC 9(08).
             10 COBR-DIG-CONTRATO       PIC X(01).
             10 COBR-NUM-CONTRATO       PIC 9(07).
          05 COBR-CHAVE-ORIGEM.
             10 COBR-DATA-ORIG          PIC X(10).
             10 COBR-ROTINA-ORIG        PIC X(04).
             10 COBR-NUM-ARQ-ORIG       PIC 9(02).
             10 COBR-SEQ-ARQ-ORIG       PIC 9(11).
             10 COBR-TARIFA-ORIG        PIC 9(05).
          05 COBR-CGC-CPF.
ST2507*      10 COBR-CGC-CPF-MUNERO     PIC 9(09).
ST2507*      10 COBR-CGC-CPF-FILIAL     PIC 9(05).
ST2507       10 COBR-CGC-CPF-MUNERO     PIC X(09).
ST2507       10 COBR-CGC-CPF-FILIAL     PIC X(04).
             10 COBR-CGC-CPF-CONTROLE   PIC 9(02).
          05 COBR-DADOS-DEBITO.
             10 COBR-BANCO-DEB          PIC 9(03).
             10 COBR-AGENCIA-DEB        PIC 9(05).
             10 COBR-CONTA-DEB          PIC 9(07).
             10 COBR-DIGITO-DEB         PIC X(01).
             10 COBR-CARTEIRA-DEB       PIC 9(05).
             10 COBR-COD-LCTO-DEB       PIC 9(05).
             10 COBR-COD-MOEDA-DEB      PIC 9(05).
             10 COBR-RAZAO.
                15 COBR-GRUPO-DEB       PIC 9(03).
                15 COBR-SUBGRUPO-DEB    PIC 9(03).
             10 COBR-VALOR-DEBITAR      PIC 9(11)V9(2).
          05 COBR-DETALHES-COBRANCA.
             10 COBR-SITUACAO-EVENTO    PIC 9(02).
             10 COBR-TIPO-COBRANCA      PIC 9(02).
             10 COBR-TIPO-ARQUIVO       PIC 9(02).
             10 COBR-DEB-PARCIAL        PIC 9(02).
             10 COBR-TIPO-SALDO         PIC 9(02).
             10 COBR-TIPO-CONTA         PIC 9(01).
             10 COBR-INCONDICIONAL      PIC X(01).
             10 COBR-ACEITA-PARCELADO   PIC X(01).
             10 COBR-PARCIAL-ULT-REPIQ  PIC X(01).
             10 COBR-VALOR-MINIMO       PIC 9(11)V9(2).
             10 COBR-BANCO-ORIG         PIC 9(03).
             10 COBR-AGENCIA-ORIG       PIC 9(05).
             10 COBR-DATA-EVNTO-ORIG    PIC 9(08).
             10 COBR-SEG-LINHA-EXTRATO  PIC X(32).
          05 COBR-RETORNO.
             10 COBR-JOB                PIC X(08).
             10 COBR-PROGRAMA           PIC X(08).
             10 COBR-RECURSO            PIC X(08).
             10 COBR-COD-RETORNO        PIC X(03).
             10 COBR-VALOR-DEBITADO     PIC 9(11)V9(2).
             10 COBR-DESCRICAO          PIC X(50).
          05 COBR-TRAG-EXT.
             10 COBR-CENTRALIZ          PIC X(01).
             10 COBR-AGENCIA-RET        PIC 9(05).
             10 COBR-CONTA-RET          PIC 9(07).
          05 COBR-DOCTO-EXTRT           PIC 9(09) COMP-3.
          05 COBR-NUM-PARC              PIC 9(03) COMP-3.
BI0813    05 COBR-ULTIMO-REPIQUE        PIC X(01).
      *
      *****************************************************************
      *    DESCRICAO DOS CAMPOS                                       *
      *****************************************************************
      *
      *REG-COBRANCA.
      * COBR-CHAVE-COBRANCA.   - CHAVE REGISTRO MORA OU POUP
      *  COBR-DATA-ENVIO       - DATA ENVIO DO EVENTO P/ COBRANCA
      *  COBR-DIG-CONTRATO     - CARACTER DE 0 A Z PARA CADA DIGITO O
      *                          O CONTRATO VARIA DE 0 A 9999999
      *  COBR-NUM-CONTRATO     - NUMERO SEQUENCIAL REG ENVIADOS NA DATA
      *                          EM CASO DE REGISTRO A UNIFICAR 1 OU  2
      *                          POSSUIRA O MESMO NUMERO DO REGISTRO
      *                          UNIFICADO 3 OU 4
      * COBR-CHAVE-ORIGEM.     - CHAVE REGISTRO DE ORIGEM DO EVENTO
      *  COBR-DATA-ORIG        - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  COBR-ROTINA-ORIG      - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  COBR-NUM-ARQ-ORIG     - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  COBR-SEQ-ARQ-ORIG     - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  COBR-TARIFA-ORIG      - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      * COBR-CGC-CPF.          - CPF OU CNPJ DO CLIENTE A DEBITAR
      *  COBR-CGC-CPF-MUNERO   -
      *  COBR-CGC-CPF-FILIAL   -
      *  COBR-CGC-CPF-CONTROLE -
      * COBR-DADOS-DEBITO      - DADOS DO CLIENTE A DEBITAR
      *  COBR-BANCO-DEB        - DO CLIENTE OU DA AG. CENTTRALIZADORA
      *  COBR-AGENCIA-DEB      - DO CLIENTE OU DA AG. CENTTRALIZADORA
      *  COBR-CONTA-DEB        -
      *  COBR-DIGITO-DEB       -
      *  COBR-CARTEIRA-DEB     -
      *  COBR-COD-LCTO-DEB     -
      *  COBR-COD-MOEDA-DEB    -
      *  COBR-RAZAO            - RAZAO DA AGENCIA/CONTA A DEBITAR
      *   COBR-GRUPO-DEB       -
      *   COBR-SUBGRUPO-DEB    -
      *  COBR-VALOR-DEB        - VALOR A DEBITAR
      * COBR-DETALHES-COBRANCA - INFORMACOES COMPLEMENTARES P/ COBRANCA
      *  COBR-SITUACAO-EVENTO  - CSTI-EVNTO-RECBD - 2-AGENDADO/3-REPIQUE
      *  COBR-TIPO-COBRANCA    - TIPO DE COBRANCA
      *                           1 - INDIVIDUAL
      *                           2 - UNIFICADO
      *                           3 - CONSOLIDADO
      *                           4 - DETALHE UNIFICADO
      *                           5 - DETALHE CONSOLIDADO
      *  COBR-TIPO-ARQUIVO     - TRATAMENTO DO ARQUIVO ENVIADO
      *                           1 - COBRANCA VIA MORA
      *                           2 - REPIQUE  VIA MORA
      *                           3 - COBRANCA / REPIQUE POUP
      *  COBR-DEB-PARCIAL      - ACEITA DEBITO PARCIAL
      *                           1 - SIM
      *                           2 - NAO
      *  COBR-TIPO-SALDO       - TIPO DE SALDO A DEBITAR
      *                           1 - VINCULADO
      *                           2 - DISPONIVEL
      *  COBR-TIPO-CONTA       - TIPO DE CONTA A DEBITAR
      *                           1 - CONTA CORRENTE
      *                           2 - CONTA POUPANCA
      *  COBR-INCONDICIONAL    - IGNORA DEB-PARCIAL E COBRA TOTAL
      *                           S - SIM
      *                           N - NAO
      *  COBR-ACEITA-PARCELADO -  ACETIA PARCELAMENTO DO DEBITO
      *                           S - SIM
      *                           N - NAO
      *  COBR-PARCIAL-ULT-REPIQ- ACEITA DEBITO PARCIAL NO ULTIMO REPIQUE
      *                           1 - SIM
      *                           2 - NAO
      *  COBR-VALOR-MINIMO     - VALOR MINIMO A DEBITAR
      *  COBR-BANCO-ORIG       - BANCO DE ORIGEM DO EVENTO
      *  COBR-AGENCIA-ORIG     - AGENCIA DE ORIGEM DO EVENTO
      *  COBR-DATA-EVNTO-ORIG  - DATA DO EVENTO
      *  COBR-SEG-LINHA-EXTRATO- A DEMONSTRAR NO EXTRATO DO CLIENTE
      * COBR-RETORNO           - RETORNO DOS MODULOS ACESSOS E SISTEMAS
      *                          VALOR DEBITADO MORA/POUP OU MSG ERROS
      *  COBR-JOB              - JOB DA OCORRENCIA
      *  COBR-PROGRAMA         - PROGRAMA DA OCORRENCIA
      *  COBR-RECURSO          - SISTEMA, TAB, MODULO OU PGM CORRENCIA
      *  COBR-COD-RETORNO      - RETORNADO PELO RECURSO
      *  COBR-VALOR-DEBITADO   - PELO MORA OU POUP
      *  COBR-DESCRICAO        - DA OCORRENCIA
      *  COBR-TRAG-EXT         - PARA CASO HAJA TRAG NO MORA OU POUP
      *   COBR-CENTRALIZ       - AG/CTA. COBRANCA CENTRALIZADORA S/N
      *   COBR-AGENCIA-RET     - AGENCIA COBR.RETORNADA DO MORA/POUP
      *   COBR-CONTA-RET       - CONTA COBR.RETORNADA DO MORA/POUP
      *  COBR-DOCTO-EXTRT      - NUMERO DO DOCUMENTO
      *   COBR-NUM-PARC        - NUMERO DA PARCELA
BI0813*   COBR-ULTIMO-REPIQUE  - INDICA SE EH ULTIMO REPIQUE 'S' OU 'N'
      *
      *****************************************************************
      *
