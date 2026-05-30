      ******************************************************************
      *  RESOLUCAO 4734 - CLIENTES MASSIVOS                            *
      ******************************************************************
      *  NOME DO BOOK..: I#BVVEFY                                      *
      *  DESCRICAO.....: AREA DO PARAMETRO ESVB PARA GRANDES VOLUMES   *
      *  TAMANHO.......: 300 BYTES                                     *
      *----------------------------------------------------------------*
      *  IND-ORIG-MASSV: INDICADOR DA ORIGEM DA OPERACAO MASSIVA       *
      *                  'C' CANAL (MOBILE / NET EMPRESA)              *
      *                  'G' TERMINAL GERENCIAL                        *
      *  NSMULA-OPER...: NUMERO DE SIMULACAO DA OPERACAO               *
      *                  DEVE SER PREENCHIDO QUANDO CHAMADO PELO CANAL *
      *  OPERACAO......: NUMERO DA OPERACAO                            *
      *                  DEVE SER PREENCHIDO QUANDO CHAMADO PELO TG    *
      ******************************************************************
      *                                                                *
       01  BVVEFY-PARAMETRO.
           05 BVVEFY-DATA            PIC  X(010)         VALUE SPACES.
           05 BVVEFY-HORA            PIC  X(008)         VALUE SPACES.
           05 BVVEFY-PROTOCOLO       PIC  X(050)         VALUE SPACES.
           05 BVVEFY-AGENCIA         PIC  9(005)         VALUE ZEROS.
           05 BVVEFY-CONTA           PIC  9(013)         VALUE ZEROS.
           05 BVVEFY-CANAL           PIC  X(004)         VALUE SPACES.
           05 BVVEFY-CONTRATO        PIC  X(019)         VALUE SPACES.
           05 BVVEFY-IND-I-A         PIC  X(001)         VALUE SPACES.
           05 BVVEFY-DT-FIM          PIC  X(010)         VALUE SPACES.
           05 BVVEFY-VLR-TOT         PIC  9(015)V99      VALUE ZEROS.
           05 BVVEFY-ORIG-MASSV      PIC  X(001)         VALUE SPACES.
           05 BVVEFY-NSMULA          PIC  9(009)         VALUE ZEROS.
           05 BVVEFY-OPER-ANO        PIC  9(004)         VALUE ZEROS.
           05 BVVEFY-OPER-SEQ        PIC  9(009)         VALUE ZEROS.
           05 FILLER                 PIC  X(140)         VALUE SPACES.
