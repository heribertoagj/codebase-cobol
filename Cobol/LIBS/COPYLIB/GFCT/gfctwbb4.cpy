      *****************************************************************
      * NOME DA INC - GFCTWBB4                                        *
      * DESCRICAO   - LAYOUT DO ARQUIVO DE REGISTROS ENVIADOS PARA    *
      *               COBRANCA FORMATO ZONADO                         *
ST2507* TAMANHO     - 0303                                            *
      * DATA        - ABR/2012                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 BB4-REG.
          05 BB4-CHAVE-COBRANCA.
             10 BB4-DATA-ENVIO         PIC 9(08).
             10 BB4-DIG-CONTRATO       PIC X(01).
             10 BB4-NUM-CONTRATO       PIC 9(07).
          05 BB4-CHAVE-ORIGEM.
             10 BB4-DATA-ORIG          PIC X(10).
             10 BB4-ROTINA-ORIG        PIC X(04).
             10 BB4-NUM-ARQ-ORIG       PIC 9(02).
             10 BB4-SEQ-ARQ-ORIG       PIC 9(11).
             10 BB4-TARIFA-ORIG        PIC 9(05).
          05 BB4-CGC-CPF.
ST2507*      10 BB4-CGC-CPF-MUNERO     PIC 9(09).
ST2507*      10 BB4-CGC-CPF-FILIAL     PIC 9(05).
ST2507       10 BB4-CGC-CPF-MUNERO     PIC X(09).
ST2507       10 BB4-CGC-CPF-FILIAL     PIC X(04).
             10 BB4-CGC-CPF-CONTROLE   PIC 9(02).
          05 BB4-DADOS-DEBITO.
             10 BB4-BANCO-DEB          PIC 9(03).
             10 BB4-AGENCIA-DEB        PIC 9(05).
             10 BB4-CONTA-DEB          PIC 9(07).
             10 BB4-DIGITO-DEB         PIC X(01).
             10 BB4-CARTEIRA-DEB       PIC 9(05).
             10 BB4-COD-LCTO-DEB       PIC 9(05).
             10 BB4-COD-MOEDA-DEB      PIC 9(05).
             10 BB4-RAZAO.
                15 BB4-GRUPO-DEB       PIC 9(03).
                15 BB4-SUBGRUPO-DEB    PIC 9(03).
             10 BB4-VALOR-DEBITAR      PIC 9(11)V9(2).
          05 BB4-DETALHES-COBRANCA.
             10 BB4-SITUACAO-EVENTO    PIC 9(02).
             10 BB4-TIPO-COBRANCA      PIC 9(02).
             10 BB4-TIPO-ARQUIVO       PIC 9(02).
             10 BB4-DEB-PARCIAL        PIC 9(02).
             10 BB4-TIPO-SALDO         PIC 9(02).
             10 BB4-TIPO-CONTA         PIC 9(01).
             10 BB4-INCONDICIONAL      PIC X(01).
             10 BB4-ACEITA-PARCELADO   PIC X(01).
             10 BB4-PARCIAL-ULT-REPIQ  PIC X(01).
             10 BB4-VALOR-MINIMO       PIC 9(11)V9(2).
             10 BB4-BANCO-ORIG         PIC 9(03).
             10 BB4-AGENCIA-ORIG       PIC 9(05).
             10 BB4-DATA-EVNTO-ORIG    PIC 9(08).
             10 BB4-SEG-LINHA-EXTRATO  PIC X(32).
          05 BB4-RETORNO.
             10 BB4-JOB                PIC X(08).
             10 BB4-PROGRAMA           PIC X(08).
             10 BB4-RECURSO            PIC X(08).
             10 BB4-COD-RETORNO        PIC X(03).
             10 BB4-VALOR-DEBITADO     PIC 9(11)V9(2).
             10 BB4-DESCRICAO          PIC X(50).
          05 BB4-TRAG-EXT.
             10 BB4-CENTRALIZ          PIC X(01).
             10 BB4-AGENCIA-RET        PIC 9(05).
             10 BB4-CONTA-RET          PIC 9(07).
          05 BB4-DOCTO-EXTRT           PIC 9(09).
          05 BB4-NUM-PARC              PIC 9(03).
      *****************************************************************
      *
