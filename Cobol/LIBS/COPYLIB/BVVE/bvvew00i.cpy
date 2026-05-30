      ******************************************************************
      * BVVEW00I BLOCO DE CONTROLE PARA CHAMADAS A SERVICOS FUNCIONAIS *
      ******************************************************************
       05 BVVEW00I-HEADER.
          10 BVVEW00I-COD-LAYOUT            PIC  X(08) VALUE 'BVVEW00I'.
          10 BVVEW00I-TAM-LAYOUT            PIC  9(05) VALUE  027.
       05 BVVEW00I-BLOCO-RETORNO.
          10 BVVEW00I-COD-RETORNO           PIC  9(02).
          10 BVVEW00I-COD-ERRO              PIC  X(04).
          10 BVVEW00I-COD-MENSAGEM          PIC  X(08).
