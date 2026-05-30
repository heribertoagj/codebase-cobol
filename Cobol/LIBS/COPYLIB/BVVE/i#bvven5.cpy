      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      ******************************************************************
      *  NOME DO BOOK....: I#BVVEN5                                    *
      *  DESCRICAO.......: OPERACOES EM LOTE - ALTERACAO E INCLUSAO    *
      *                    PREPARA ARQUIVO PARA ENVIO DO ARRC022       *
      *  TAMANHO.........: 200 BYTES                                   *
      *----------------------------------------------------------------*
      *  COD-OPERACAO : CONTRATO LOTE / CONTRATO BRADSECO              *
      *  COD-OPER-CIP : CONTRATO DEVOLVIDO PELA CIP                    *
      *  DATAS        : DD.MM.AAAA                                     *
      ******************************************************************
      *                                                                *
       01  REG-BVVEN5.
           03 BVVEN5-DADOS-CONTRATO.
              05 BVVEN5-AGENCIA                     PIC  9(005).
              05 BVVEN5-CONTA                       PIC  9(013).
              05 BVVEN5-COD-OPERACAO                PIC  X(019).
              05 BVVEN5-IND-I-A                     PIC  X(001).
              05 BVVEN5-DT-VCTO-OPER                PIC  X(010).
              05 BVVEN5-VLR-TOT-OPER                PIC  9(015)V99.
           03 BVVEN5-DADOS-UR.
              05 BVVEN5-PRODUTO                     PIC  9(005).
              05 BVVEN5-VLR-OPER-PRODUTO            PIC  9(015)V99.
              05 BVVEN5-DT-UR                       PIC  X(010).
           03 BVVEN5-CRED                           PIC  9(004).
           03 BVVEN5-CNPJ-CREDENC                   PIC  9(014).
           03 BVVEN5-BAND-VISAO                     PIC  9(003).
           03 BVVEN5-ARRANJO                        PIC  X(003).
      *    PARA CPF FORMATAR COM ESPAÁO A DIREITA
           03 BVVEN5-CNPJ-USUAR-FNAL                PIC  X(014).
           03 BVVEN5-COD-OPER-CIP                   PIC  X(019).
           03 BVVEN5-DT-INI-GRAV                    PIC  X(010).
           03 BVVEN5-DT-FIM-GRAV                    PIC  X(010).
           03 BVVEN5-IND-RENEG                      PIC  X(001).
           03 BVVEN5-CONTRATO-RENEG                 PIC  X(019).
           03 FILLER                                PIC  X(006).
