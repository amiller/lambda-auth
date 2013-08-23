#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <openssl/sha.h>
#include <sys/time.h>

#define LEAF_SIZE 1024
typedef unsigned char leaf[LEAF_SIZE];
typedef unsigned char digest[20];
typedef struct {
    digest  left;
    digest right;
} node;

typedef struct {
    int height;
    node *root;
    leaf *data;
} tree;

typedef struct {
    leaf item;
    digest *siblings;
} path;

static void print_hex(const unsigned char *d) {
    for (int i = 0; i < 20; i++) printf("%02x", d[i]);
    printf("\n");
}

static inline node *layer(const tree *t, int k) {
    return t->root + (1<<k) - 1;
}

static void hash_leaf(const leaf l, digest d) {
    //printf("leaf:");
    //print_hex((unsigned char*) l);
    SHA1((unsigned char*) l, sizeof(leaf), d);
}

static void hash_node(const node *n, digest d) {
    //print_hex(n->left);
    //print_hex(n->right);
    SHA1((unsigned char *) n, sizeof(node), d);
}


static tree *build_tree(int height) {
    int n = 1 << height;
    tree *t = (tree *) malloc(sizeof(tree));
    t->height = height;
    t->root = malloc((n-1)*sizeof(node));
    t->data = malloc(n*sizeof(leaf));

    // Load the leaf data from a file
    char filename[256];
    sprintf(filename, "data/leaves_%03d.dat", height);
    printf("opening: %s, reading %d bytes\n", filename, n*sizeof(leaf));
    FILE *file = fopen(filename, "rb");
    assert (fread(t->data, sizeof(leaf), n, file) == n);

    // Process each layer of the tree, starting with the leaves
    node *layer0 = layer(t, height-1);
    node *layer1 = 0;
    for (int i = 0; i < n/2; i++) {
        hash_leaf(t->data[2*i+0], layer0[i]. left);
        hash_leaf(t->data[2*i+1], layer0[i].right);
    }
    for (int k = height-2; k >= 0; k--) {
        layer1 = layer0;
        layer0 = layer(t, k);
        for (int i = 0; i < (1<<k); i++) {
            hash_node(&layer1[2*i+0], layer0[i]. left);
            hash_node(&layer1[2*i+1], layer0[i].right);
        }
    }

    return t;
}

static void free_tree(tree *t) {
    free(t->root);
    free(t->data);
    free(t);
}

path *alloc_path(int height) {
    path *p = (path *) malloc(sizeof(path));
    p->siblings = (digest *) malloc(sizeof(digest) * height);
    return p;
}

void build_path(tree *t, int idx, path *p) {
    assert(idx >= 0 && idx < (1<<t->height));
    memcpy(p->item, t->data[idx], sizeof(leaf));
    for (int k = 0; k < t->height; k++) {
        node *layer0 = layer(t, t->height-1-k);
        int bit = idx % 2;
        if (!bit) {
            memcpy(p->siblings[k], layer0[idx/2].right, sizeof(digest));
        } else {
            memcpy(p->siblings[k], layer0[idx/2].left, sizeof(digest));
        }
        idx /= 2;
    }
}

void read_path(int height, FILE *file, path *p) {
    assert (fread(p->item, sizeof(leaf), 1, file) == 1);
    assert (fread(p->siblings, sizeof(digest), height, file) == height);
}

void write_path(int height, FILE *file, path *p) {
    assert (fwrite(p->item, sizeof(leaf), 1, file) == 1);
    assert (fwrite(p->siblings, sizeof(digest), height, file) == height);
}

void *free_path(path *p) {
    free(p->siblings);
    free(p);
}

void validate_path(int height, digest root, int idx, path *p) {
    static node n1, n2;
    node *ping = &n1;
    node *pong = &n2;
    if (!(idx & 1))
        hash_leaf(p->item, ping->left);
    else
        hash_leaf(p->item, ping->right);

    for (int k = 0; k < height; k++) {
        int bit = idx & 1;
        if (!bit) {
            memcpy(ping->right, p->siblings[k], sizeof(digest));
        } else {
            memcpy(ping->left , p->siblings[k], sizeof(digest));
        }
        if (!((idx >> 1) & 1)) {
            hash_node(ping, pong->left);
        } else {
            hash_node(ping, pong->right);
        }
        idx >>= 1;
        node *tmp = ping;
        ping = pong;
        pong = tmp;
    }
    assert(!strncmp(ping->left,root,sizeof(digest)));
}

struct timeval tv;

void timer_reset() {
    gettimeofday(&tv, NULL);
}

int timer_sample() {
    struct timeval now;
    gettimeofday(&now, NULL);
    return (now.tv_sec*1000 + now.tv_usec/1000) - 
        (tv.tv_sec*1000 + tv.tv_usec/1000);
}


void prover_test(int k, int rep, int iter) {
    tree *t = build_tree(k);
    int n = 1<<k;
    char filename[256];

    for (int r = 0; r < rep; r++) {
        sprintf(filename, "data/proof_cmerkle_%03d.dat", k);
        FILE *f = fopen(filename, "wb");
        srand(0x7071);
        timer_reset();
        path *p = alloc_path(t->height);
        for (int i = 0; i < iter; i++) {
            int idx = rand() % n;
            build_path(t, idx, p);
            write_path(t->height, f, p);
        }
        fclose(f);
        free_path(p);
        int ms = timer_sample();
        printf("(prover) lookup in size 2^%d, x%d: %0.3f seconds\n", k, iter, ms / 1000.0);
    }
    free_tree(t);
}

 void verifier_test(int k, int rep, int iter) {
    digest root;
    tree *t = build_tree(k);
    hash_node(t->root, root);
    free_tree(t);
    int n = 1<<k;
    char filename[256];

    for (int r = 0; r < rep; r++) {
        sprintf(filename, "data/proof_cmerkle_%03d.dat", k);
        FILE *f = fopen(filename, "rb");
        srand(0x7071);
        path *p = alloc_path(k);
        timer_reset();
        for (int i = 0; i < iter; i++) {
            int idx = rand() % n;
            read_path(k, f, p);
            validate_path(k, root, idx, p);
        }
        fclose(f);
        int ms = timer_sample();
        free_path(p);
        printf("(verifier) lookup in size 2^%d, x%d: %0.3f seconds\n", k, iter, ms / 1000.0);
    }
}

int main(int argc, char *argv[]) {
    tree *t = build_tree(4);
    digest root;
    hash_node(t->root, root);

    for (int k = 4; k <= 18; k++) {
        prover_test(k, 1, 100000);
    }

    for (int k = 4; k <= 18; k++) {
        verifier_test(k, 7, 100000);
    }
}
