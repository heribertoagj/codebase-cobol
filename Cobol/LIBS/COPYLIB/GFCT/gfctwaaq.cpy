      *****************************************************************
      * NOME DA INC - GFCTWAAQ                                        *
      * DESCRICAO   - LAYOUT DO ARQUIVO DE  CONFERENCIA  DA COBRANCA  *
      *               CONTEM DADOS DE IDA E VOLTA DA COBRANCA, FITA4  *
      *               ARQUIVO DE DEBITO E ERROS                       *
      * TAMANHO     - 0400                                            *
      * DATA        - 07.2008                                         *
      * RESPONSAVEL - WAGNER - PROCWORK                               *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 REG-CONFERENCIA.
          05 CONF-CHAVE-COBRANCA.
             10 CONF-DATA-ENVIO         PIC 9(08).
             10 CONF-DIG-CONTRATO       PIC X(01).
             10 CONF-NUM-CONTRATO       PIC 9(07).
          05 CONF-CHAVE-ORIGEM.
             10 CONF-DATA-ORIG          PIC X(10).
             10 CONF-ROTINA-ORIG        PIC X(04).
             10 CONF-NUM-ARQ-ORIG       PIC 9(02).
             10 CONF-SEQ-ARQ-ORIG       PIC 9(11).
             10 CONF-TARIFA-ORIG        PIC 9(05).
          05 CONF-CGC-CPF.
ST2507*      10 CONF-CGC-CPF-MUNERO     PIC 9(09).
ST2507*      10 CONF-CGC-CPF-FILIAL     PIC 9(05).
ST2507       10 CONF-CGC-CPF-MUNERO     PIC X(09).
ST2507       10 CONF-CGC-CPF-FILIAL     PIC X(04).
             10 CONF-CGC-CPF-CONTROLE   PIC 9(02).
          05 CONF-DADOS-DEBITO.
             10 CONF-BANCO-DEB          PIC 9(03).
             10 CONF-AGENCIA-DEB        PIC 9(05).
             10 CONF-CONTA-DEB          PIC 9(07).
             10 CONF-DIGITO-DEB         PIC X(01).
             10 CONF-CARTEIRA-DEB       PIC 9(05).
             10 CONF-COD-LCTO-DEB       PIC 9(05).
             10 CONF-COD-MOEDA-DEB      PIC 9(05).
             10 CONF-RAZAO.
                15 CONF-GRUPO-DEB       PIC 9(03).
                15 CONF-SUBGRUPO-DEB    PIC 9(03).
             10 CONF-VALOR-DEBITAR      PIC 9(11)V9(2).
          05 CONF-DETALHES-COBRANCA.
             10 CONF-SITUACAO-EVENTO    PIC 9(02).
             10 CONF-TIPO-COBRANCA      PIC 9(02).
             10 CONF-TIPO-ARQUIVO       PIC 9(02).
             10 CONF-DEB-PARCIAL        PIC 9(02).
             10 CONF-TIPO-SALDO         PIC 9(02).
             10 CONF-TIPO-CONTA         PIC 9(01).
             10 CONF-INCONDICIONAL      PIC X(01).
             10 CONF-ACEITA-PARCELADO   PIC X(01).
             10 CONF-PARCIAL-ULT-REPIQ  PIC X(01).
             10 CONF-VALOR-MINIMO       PIC 9(11)V9(2).
             10 CONF-BANCO-ORIG         PIC 9(03).
             10 CONF-AGENCIA-ORIG       PIC 9(05).
             10 CONF-DATA-EVNTO-ORIG    PIC 9(08).
             10 CONF-SEG-LINHA-EXTRATO  PIC X(32).
          05 CONF-RETORNO.
             10 CONF-JOB                PIC X(08).
             10 CONF-PROGRAMA           PIC X(08).
             10 CONF-RECURSO            PIC X(08).
             10 CONF-COD-RETORNO        PIC X(03).
             10 CONF-VALOR-DEBITADO     PIC 9(11)V9(2).
             10 CONF-DESCRICAO          PIC X(50).
          05 CONF-TRAG-EXT.
             10 CONF-CENTRALIZ          PIC X(01).
             10 CONF-AGENCIA-RET        PIC 9(05).
             10 CONF-CONTA-RET          PIC 9(07).
          05 CONF-DADOS-EXTERNOS.
             10 CONF-QUANT              PIC 9(01) VALUE 1.
             10 CONF-TRAG-SN            PIC X(01).
             10 CONF-FITA4.
                15 CONF-F4-SN           PIC X(01).
                15 CONF-F4-AGEN         PIC 9(05).
                15 CONF-F4-CONTA        PIC 9(07).
                15 CONF-F4-VLR          PIC 9(11)V9(2).
             10 CONF-ARQDEBIT.
                15 CONF-DB-SN           PIC X(01).
                15 CONF-DB-AGEN         PIC 9(05).
                15 CONF-DB-CONTA        PIC 9(07).
                15 CONF-DB-VLR          PIC 9(11)V9(2).
             10 CONF-ERRO.
                15 CONF-ERR-SN          PIC X(01).
                15 CONF-ERR-REL-SN      PIC X(01).
                15 CONF-ERR-DESCR       PIC X(50).
             10 FILLER                  PIC X(02).
      *
      *****************************************************************
      *    DESCRICAO DOS CAMPOS                                       *
      *****************************************************************
      *
      *REG-CONFERENCIA.
      * CONF-CHAVE-COBRANCA.   - CHAVE REGISTRO MORA OU POUP
      *  CONF-DATA-ENVIO       - DATA ENVIO DO EVENTO P/ COBRANCA
      *  CONF-DIG-CONTRATO     - CARACTER DE 0 A Z PARA CADA DIGITO O
      *                          O CONTRATO VARIA DE 0 A 9999999
      *  CONF-NUM-CONTRATO     - NUMERO SEQUENCIAL REG ENVIADOS NA DATA
      *                          EM CASO DE REGISTRO A UNIFICAR 1 OU  2
      *                          POSSUIRA O MESMO NUMERO DO REGISTRO
      *                          UNIFICADO 3 OU 4
      * CONF-CHAVE-ORIGEM.     - CHAVE REGISTRO DE ORIGEM DO EVENTO
      *  CONF-DATA-ORIG        - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  CONF-ROTINA-ORIG      - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  CONF-NUM-ARQ-ORIG     - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  CONF-SEQ-ARQ-ORIG     - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      *  CONF-TARIFA-ORIG      - VINDO DAS TABELAS DE MOVIMENTO DO GFCT
      * CONF-CGC-CPF.          - CPF OU CNPJ DO CLIENTE A DEBITAR
      *  CONF-CGC-CPF-MUNERO   -
      *  CONF-CGC-CPF-FILIAL   -
      *  CONF-CGC-CPF-CONTROLE -
      * CONF-DADOS-DEBITO      - DADOS DO CLIENTE A DEBITAR
      *  CONF-BANCO-DEB        - DO CLIENTE OU DA AG. CENTTRALIZADORA
      *  CONF-AGENCIA-DEB      - DO CLIENTE OU DA AG. CENTTRALIZADORA
      *  CONF-CONTA-DEB        -
      *  CONF-DIGITO-DEB       -
      *  CONF-CARTEIRA-DEB     -
      *  CONF-COD-LCTO-DEB     -
      *  CONF-COD-MOEDA-DEB    -
      *  CONF-RAZAO            - RAZAO DA AGENCIA/CONTA A DEBITAR
      *   CONF-GRUPO-DEB       -
      *   CONF-SUBGRUPO-DEB    -
      *  CONF-VALOR-DEB        - VALOR A DEBITAR
      * CONF-DETALHES-COBRANCA - INFORMACOES COMPLEMENTARES P/ COBRANCA
      *  CONF-SITUACAO-EVENTO  - CSTI-EVNTO-RECBD - 2-AGENDADO/3-REPIQUE
      *  CONF-TIPO-COBRANCA    - TIPO DE COBRANCA
      *                           01 - INDIVIDUAL
      *                           04 - DETALHE UNIFICADO
      *                           05 - DETALHE CONSOLIDADO
      *                           08 - DETALHE AGRUPADO
      *                           10 - DETALHE PARTE EMPRESA
      *  CONF-TIPO-ARQUIVO     - TRATAMENTO DO ARQUIVO ENVIADO
      *                           1 - COBRANCA VIA MORA
      *                           2 - REPIQUE  VIA MORA
      *                           3 - COBRANCA / REPIQUE POUP
      *  CONF-DEB-PARCIAL      - ACEITA DEBITO PARCIAL
      *                           1 - SIM
      *                           2 - NAO
      *  CONF-TIPO-SALDO       - TIPO DE SALDO A DEBITAR
      *                           1 - VINCULADO
      *                           2 - DISPONIVEL
      *  CONF-TIPO-CONTA       - TIPO DE CONTA A DEBITAR
      *                           1 - CONTA CORRENTE
      *                           2 - CONTA POUPANCA
      *  CONF-INCONDICIONAL    - IGNORA DEB-PARCIAL E COBRA TOTAL
      *                           S - SIM
      *                           N - NAO
      *  CONF-ACEITA-PARCELADO -  ACETIA PARCELAMENTO DO DEBITO
      *                           S - SIM
      *                           N - NAO
      *  CONF-PARCIAL-ULT-REPIQ- ACEITA DEBITO PARCIAL NO ULTIMO REPIQUE
      *                           1 - SIM
      *                           2 - NAO
      *  CONF-VALOR-MINIMO     - VALOR MINIMO A DEBITAR
      *  CONF-BANCO-ORIG       - BANCO DE ORIGEM DO EVENTO
      *  CONF-AGENCIA-ORIG     - AGENCIA DE ORIGEM DO EVENTO
      *  CONF-DATA-EVNTO-ORIG  - DATA DO EVENTO
      *  CONF-SEG-LINHA-EXTRATO- A DEMONSTRAR NO EXTRATO DO CLIENTE
      * CONF-RETORNO           - RETORNO DOS MODULOS ACESSOS E SISTEMAS
      *                          VALOR DEBITADO MORA/POUP OU MSG ERROS
      *  CONF-JOB              - JOB DA OCORRENCIA
      *  CONF-PROGRAMA         - PROGRAMA DA OCORRENCIA
      *  CONF-RECURSO          - SISTEMA, TAB, MODULO OU PGM CORRENCIA
      *  CONF-COD-RETORNO      - RETORNADO PELO RECURSO
      *  CONF-VALOR-DEBITADO   - PELO MORA OU POUP
      *  CONF-DESCRICAO        - DA OCORRENCIA
      *  CONF-TRAG-EXT         - PARA CASO HAJA TRAG NO MORA OU POUP
      *   CONF-CENTRALIZ       - AG/CTA. COBRANCA CENTRALIZADORA S/N
      *   CONF-AGENCIA-RET     - AGENCIA COBR.RETORNADA DO MORA/POUP
      *   CONF-CONTA-RET       - CONTA COBR.RETORNADA DO MORA/POUP
      *   CONF-DADOS-EXTERNOS  - DADOS ATUALIZACAO EVENTO NAS BASES
      *    CONF-QUANT          - (SEMPRE 1) VLR USADO P/ SOMAR EVENTOS
      *    CONF-TRAG-SN        - (S) - MORA/POUP RETORNOU AG/CTA DIFE-
      *                                RENTE DA ENVIADA.
      *    CONF-FITA4          - DADOS DA FITA4
      *     CONF-F4-SN         - EVENTO GEROU FITA4 S/N
      *     CONF-F4-AGEN       - AGENCIA FITA4
      *     CONF-F4-CONTA      - CONTA FITA4
      *     CONF-F4-VLR        - VALOR ENVIADO FITA4
      *    CONF-TAB092         - DADOS GRAVADOS NO ARQDEBIT
      *     CONF-DB-SN         - EVENTO GEROU ARQDEBIR S/N
      *     CONF-DB-AGEN       - AGENCIA ARQDEBIT
      *     CONF-DB-CONTA      - CONTA ARQDEBIT
      *     CONF-DB-VLR        - VALOR ARQDEBIT
      *    CONF-ERRO.
      *     CONF-ERR-SN        - HOUVE ERRO NO PROCESSO S/N
      *     CONF-ERR-REL-S/N   - ERRO REGISTRADO NO ARQOCORR S/N
      *     CONF-ERR-DESCR     - DESCRICAO DO ERRO
      *****************************************************************

