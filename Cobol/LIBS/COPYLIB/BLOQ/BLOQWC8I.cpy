           05 BLOQWC8I-HEADER.
              10 BLOQWC8I-COD-LAYOUT         PIC X(08) VALUE 'BLOQWC8I'.
              10 BLOQWC8I-TAM-LAYOUT         PIC 9(05) VALUE 00474.
           05 BLOQWC8I-BLOCO-ENTRADA.
      *       1 - listar codigo bloqueio
              10 BLOQWC8I-E-CTPO-OPERACAO    PIC 9(001).  
              10 BLOQWC8I-E-BLOQ-CTA         PIC 9(002).           
           05 BLOQWC8I-BLOCO-PAGINACAO.
              10 BLOQWC8I-INDICADOR-PAGINACAO     PIC X(01).
              10 BLOQWC8I-PRI-CHAVE.
                 15 BLOQWC8I-CMOTVO-SIT-CTA-I     PIC 9(03).
              10 BLOQWC8I-ULT-CHAVE.
                 15 BLOQWC8I-CMOTVO-SIT-CTA-F     PIC 9(03).
           05 BLOQWC8I-BLOCO-SAIDA.
              10 BLOQWC8I-NUM-LINHAS              PIC 9(03).
              10 BLOQWC8I-CONSULTAS-SAIDA         OCCURS 1
                                                  TO 50 TIMES
                                                  DEPENDING ON 
                                                  BLOQWC8I-NUM-LINHAS.
                 15 BLOQWC8I-E-CMOTVO-SIT-CTA     PIC 9(03).
                 15 BLOQWC8I-S-RJUSTF-BLOQ        PIC X(255).

              
