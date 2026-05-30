      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734- GRANDES VOLUMES            *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEC4                                    *
      *  DESCRICAO.......: OPERACOES EM LOTE - ALTERACAO E INCLUSAO    *
      *                    PREPARA OPERACOES GRANDES VOLUMES PARA CIP  *
      *  TAMANHO.........: 300 BYTES                                   *
      *----------------------------------------------------------------*
      *  COD-OPERACAO : CONTRATO LOTE / CONTRATO BRADSECO              *
      *  COD-OPER-CIP : CONTRATO DEVOLVIDO PELA CIP                    *
      *  DATAS        : DD.MM.AAAA                                     *
      ******************************************************************
      *                                                                *
       01  REG-BVVEC4.
           03 BVVEC4-DADOS-CONTRATO.
              05 BVVEC4-PROTOCOLO                   PIC  X(050).
              05 BVVEC4-AGENCIA                     PIC  9(005).
              05 BVVEC4-CONTA                       PIC  9(013).
              05 BVVEC4-COD-OPERACAO                PIC  X(019).
              05 BVVEC4-IND-I-A                     PIC  X(001).
              05 BVVEC4-DT-VCTO-OPER                PIC  X(010).
              05 BVVEC4-VLR-TOT-OPER                PIC  9(015)V99.
           03 BVVEC4-DADOS-UR.
              05 BVVEC4-VLR-OPER-PRODUTO            PIC  9(015)V99.
              05 BVVEC4-DT-UR                       PIC  X(010).
              05 BVVEC4-CRED                        PIC  9(004).
              05 BVVEC4-BAND-VISAO                  PIC  9(003).
              05 BVVEC4-CNPJ-USUAR-FNAL             PIC  X(014).
           03 BVVEC4-DADOS-COMPLEMENTARES.
              05 BVVEC4-CNPJ-CREDENC                PIC  X(014).
              05 BVVEC4-ARRANJO                     PIC  X(003).
              05 BVVEC4-COD-OPER-CIP                PIC  X(019).
              05 BVVEC4-CTPO-CONTA                  PIC  X(002).
              05 BVVEC4-CPFCNPJ-TTLAR               PIC  X(014).
           03 FILLER                                PIC  X(085).
      *    PARA CPF FORMATAR COM ESPAÁO A DIREITA
