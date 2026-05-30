      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEM5                                    *
      *  DESCRICAO.......: OPERACOES EM LOTE - ALTERACAO E INCLUSAO    *
      *                    PREPARA ARQUIVO PARA ENVIO DO ARRC022       *
      *  TAMANHO.........: 350 BYTES                                   *
      *----------------------------------------------------------------*
      *  COD-OPERACAO : CONTRATO LOTE / CONTRATO BRADSECO              *
      *  COD-OPER-CIP : CONTRATO DEVOLVIDO PELA CIP                    *
      *  DATAS        : DD.MM.AAAA                                     *
      ******************************************************************
      *                                                                *
       01  REG-BVVEM5.
           03 BVVEM5-DADOS-CONTRATO.
              05 BVVEM5-AGENCIA                     PIC  9(005).
              05 BVVEM5-CONTA                       PIC  9(013).
              05 BVVEM5-COD-OPERACAO                PIC  X(019).
              05 BVVEM5-IND-I-A                     PIC  X(001).
              05 BVVEM5-DT-VCTO-OPER                PIC  X(010).
              05 BVVEM5-VLR-TOT-OPER                PIC  9(015)V99.
           03 BVVEM5-DADOS-UR.
              05 BVVEM5-PRODUTO                     PIC  9(005).
              05 BVVEM5-VLR-OPER-PRODUTO            PIC  9(015)V99.
              05 BVVEM5-DT-UR                       PIC  X(010).
           03 BVVEM5-CRED                           PIC  9(004).
           03 BVVEM5-CNPJ-CREDENC                   PIC  9(014).
           03 BVVEM5-BAND-VISAO                     PIC  9(003).
           03 BVVEM5-ARRANJO                        PIC  X(003).
      *    PARA CPF FORMATAR COM ESPAÁOS A DIREITA
           03 BVVEM5-CNPJ-USUAR-FNAL                PIC  X(014).
           03 BVVEM5-COD-OPER-CIP                   PIC  X(019).
           03 BVVEM5-DT-INI-GRAV                    PIC  X(010).
           03 BVVEM5-DT-FIM-GRAV                    PIC  X(010).
           03 BVVEM5-IND-RENEG                      PIC  X(001).
           03 BVVEM5-CONTRATO-RENEG                 PIC  X(019).
           03 BVVEM5-CTPO-CONTA                     PIC  X(002).
      *    PARA CPF FORMATAR COM ESPAÁOS A DIREITA
           03 BVVEM5-CPFCNPJ-TTLAR                  PIC  X(014).
           03 FILLER                                PIC  X(140).
