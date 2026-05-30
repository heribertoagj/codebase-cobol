       01  56-ARGUMENTOS-ENTRADA.
       05  56-COLUNAS-DA-TABELA.
           10  56-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  56-CFLIAL-CNPJ              PIC S9(5)V COMP-3.
           10  56-CCTRL-CNPJ-CPF           PIC S9(2)V COMP-3.
           10  56-CCNPJ-CPF-PROPN          PIC S9(9)V COMP-3.
           10  56-CFLIAL-CNPJ-PROPN        PIC S9(5)V COMP-3.
           10  56-CCTRL-CNPJ-PROPN         PIC S9(2)V COMP-3.
           10  56-DINIC-VGCIA-ISENC        PIC X(10).
           10  56-DFIM-VGCIA-ISENC         PIC X(10).
           10  56-CTPO-ISENC-DESC          PIC S9(3)V COMP-3.
           10  56-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  56-CCLUB-CDENT-PROPN        PIC S9(10)V COMP-3.
           10  56-HULT-ATULZ               PIC X(26).
           10  56-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  56-CTERM                    PIC X(8).
4S2511     10  56-CCNPJ-CPF-ST             PIC X(09).
4S2511     10  56-CFLIAL-CNPJ-ST           PIC X(04).
4S2511     10  56-CCTRL-CNPJ-CPF-ST        PIC S9(02)V COMP-3.
4S2511     10  56-CCNPJ-CPF-PROPN-ST       PIC X(09).
4S2511     10  56-CFLIAL-CNPJ-PROPN-ST     PIC X(04).
4S2511     10  56-CCTRL-CNPJ-PROPN-ST      PIC S9(02)V COMP-3.

       05  56-INSTRUCAO                    PIC X(10).

       01  56-RETORNO.
       05  56-COD-RETORNO                  PIC 9(04).
       05  56-MENSAGEM                     PIC X(79).
       05  56-QTDE-TOTAL                   PIC 9(09).
       05  56-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T56.
           10  56RT-CCNPJ-CPF                  PIC S9(9)V COMP-3. 
           10  56RT-NSEQ-PSSOA-DESC            PIC S9(3)V COMP-3. 
           10  56RT-CTPO-ISENC-DESC            PIC S9(3)V COMP-3. 
           10  56RT-DINIC-VGCIA-ISENC          PIC X(10).         
           10  56RT-CCNPJ-CPF-PROPN            PIC S9(9)V COMP-3. 
           10  56RT-DFIM-VGCIA-ISENC           PIC X(10).         
           10  56RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3. 
           10  56RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3. 
           10  56RT-CFLIAL-CNPJ-PROPN          PIC S9(5)V COMP-3. 
           10  56RT-CCTRL-CNPJ-PROPN           PIC S9(2)V COMP-3. 
           10  56RT-CCLUB-CDENT-PROPN          PIC S9(10)V COMP-3.
           10  56RT-HULT-ATULZ                 PIC X(26).         
           10  56RT-CFUNC-BDSCO                PIC S9(9)V COMP-3. 
           10  56RT-CTERM                      PIC X(8).          
ADERBA*******  CAMPO AUXILIAR, NAO DELETAR.                       
ADERBA     10  56RT-CABRGC-ISENC-DESC          PIC S9(3)V COMP-3. 
4S2511     10  56RT-CCNPJ-CPF-ST             PIC X(09).
4S2511     10  56RT-CFLIAL-CNPJ-ST           PIC X(04).
4S2511     10  56RT-CCTRL-CNPJ-CPF-ST        PIC S9(02)V COMP-3.
4S2511     10  56RT-CCNPJ-CPF-PROPN-ST       PIC X(09).
4S2511     10  56RT-CFLIAL-CNPJ-PROPN-ST     PIC X(04).
4S2511     10  56RT-CCTRL-CNPJ-PROPN-ST      PIC S9(02)V COMP-3.
